{-
================================================================================
 jana-sijil  -  cetak nama + No. KP pada kertas sijil (kedudukan tepat, mm)
================================================================================

 PETA FAIL INI (atas ke bawah)
   1. IMPORT          dari pustaka mana setiap komponen datang
   2. JENIS DATA      Spec (satu template), Peserta (satu baris CSV), Layout, Hasil
   3. FUNGSI TULEN    pure_*  tiada kesan sampingan (kira, semak, format teks)
        - ukuran & susun atur  : pure_layout, pure_fitSize
        - semakan data         : pure_checkRows, pure_isKP
        - laporan              : pure_report, pure_summary, pure_help
        - pembacaan config     : pure_parseConfig dan pembantunya
   4. FUNGSI IO       io_*    fail, cairo (lukis PDF), terminal
        - io_prepare : muat + semak + paparkan laporan  (kongsi oleh semak & jana)
        - io_jana    : jana sijil sebenar
        - io_ujian   : jana PDF kalibrasi
   5. TITIK MASUK     main    pilih arahan dari baris arahan

 ARAHAN (taip `jana-sijil bantuan` untuk panduan penuh)
   jana-sijil semak CONFIG CSV...
   jana-sijil ujian CONFIG OUTDIR
   jana-sijil jana  CONFIG OUTDIR CSV...

 KONVENSYEN
   pure_xxx = fungsi tulen,  io_xxx = fungsi dengan kesan sampingan.
   Semua jarak dalam mm, semua saiz font dalam pt. Pengiraan dalaman guna pt
   (1 inci = 25.4 mm = 72 pt).
-}
module Main (main) where

-- base: forM, forM_, unless, when, void (gelung dan syarat dalam monad)
import Control.Monad (forM, forM_, unless, void, when)
-- bytestring: ByteString lazy (baca fail CSV mentah; all/take/drop/pack untuk BOM dan fail kosong)
import qualified Data.ByteString.Lazy as BL
-- bytestring: B8.pack (tukar String ke ByteString untuk nama lajur CSV)
import qualified Data.ByteString.Char8 as B8
-- cassava: FromNamedRecord, decodeByName, (.:) untuk baca lajur CSV ikut nama
import qualified Data.Csv as Csv
-- base: isDigit (semak format No. KP), isSpace (buang ruang kosong)
import Data.Char (isDigit, isSpace)
-- base: dropWhileEnd, intercalate, isPrefixOf, isSuffixOf, partition
import Data.List (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf, partition)
-- containers: Map (kumpul peserta ikut template, kira bilangan)
import qualified Data.Map.Strict as Map
-- text: Text (medan CSV UTF-8), unpack (tukar ke String)
import qualified Data.Text as T
-- vector: toList (cassava pulangkan Vector)
import qualified Data.Vector as V
-- cairo: semua fungsi lukisan dan PDF di bawah
import Graphics.Rendering.Cairo
  ( FontExtents (fontExtentsAscent, fontExtentsDescent) -- ascent/descent font
  , FontSlant (FontSlantNormal)
  , FontWeight (FontWeightBold, FontWeightNormal)
  , Render                            -- monad lukisan
  , TextExtents (textExtentsXadvance) -- lebar sorongan teks
  , fontExtents, lineTo, moveTo, renderWith, selectFontFace, setFontSize
  , setLineWidth, setSourceRGB, showPage, showText, stroke
  , textExtents, withPDFSurface )
-- base: setLocaleEncoding, utf8 (supaya fail dan terminal sentiasa UTF-8)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
-- directory: createDirectoryIfMissing (buat folder output), doesFileExist (semak fail wujud)
import System.Directory (createDirectoryIfMissing, doesFileExist)
-- base: getArgs (argumen baris arahan)
import System.Environment (getArgs)
-- base: exitFailure (keluar dengan kod ralat)
import System.Exit (exitFailure)
-- filepath: (</>) gabung laluan, (<.>) tambah sambungan
import System.FilePath ((<.>), (</>))
-- base: hPutStrLn, stderr (cetak ralat)
import System.IO (hPutStrLn, stderr)
-- base: printf (format jadual)
import Text.Printf (printf)
-- base: readMaybe (tukar teks ke nombor tanpa crash)
import Text.Read (readMaybe)

-- ===================================================================
-- 2. JENIS DATA
-- ===================================================================

-- Spesifikasi LENGKAP satu template (selepas gabung [global] + [template]).
-- Semua jarak dalam mm, saiz font dalam pt.
data Spec = Spec
  { sName    :: String
  , sFont    :: String
  , sBold    :: Bool
  , sPageW   :: Double
  , sPageH   :: Double
  , sOffX    :: Double  -- pembetulan printer
  , sOffY    :: Double
  , sX1      :: Double  -- kotak: kiri
  , sY1      :: Double  -- kotak: atas
  , sX2      :: Double  -- kotak: kanan
  , sY2      :: Double  -- kotak: bawah
  , sNamaMin :: Double
  , sNamaMax :: Double
  , sKpMin   :: Double
  , sKpMax   :: Double
  , sGap     :: Double  -- jarak antara baris nama dan No. KP
  }

-- Satu baris dari CSV. Semua medan sudah dibuang ruang kosong di hujung.
data Peserta = Peserta
  { pNama     :: String
  , pNoKP     :: String
  , pTemplate :: String
  }

instance Csv.FromNamedRecord Peserta where
  parseNamedRecord r =
    Peserta <$> field "nama" <*> field "nokp" <*> field "template"
    where
      field k = pure_strip . T.unpack <$> (r Csv..: B8.pack k)

-- Fail CSV beserta barisnya (nama fail diperlukan untuk mesej ralat)
type Fail = (FilePath, [Peserta])

data Status = Muat | Dikecilkan | Terlebih
  deriving (Eq)

-- Keputusan susun atur satu sijil (semua dalam pt)
data Layout = Layout
  { lNamaSize :: Double
  , lKpSize   :: Double
  , lNamaBase :: Double  -- garis dasar nama (dari atas kertas)
  , lKpBase   :: Double  -- garis dasar No. KP
  , lStatus   :: Status
  }

-- Rekod setiap sijil, untuk ringkasan
data Hasil = Hasil
  { hNama   :: String
  , hTpl    :: String
  , hNamaSz :: Double
  , hKpSz   :: Double
  , hStatus :: Status
  }

-- ===================================================================
-- 3. FUNGSI TULEN (tiada kesan sampingan)
-- ===================================================================

-- ---- Pembantu asas ----

-- mm -> pt (1 inci = 25.4 mm = 72 pt)
pure_toPt :: Double -> Double
pure_toPt mm = mm * 72 / 25.4

-- Kedudukan (mm) + offset -> koordinat pt
pure_pos :: Spec -> (Double, Double) -> (Double, Double)
pure_pos s (x, y) = (pure_toPt (x + sOffX s), pure_toPt (y + sOffY s))

-- Kotak dalam pt (offset sudah dikenakan): (x1, y1, x2, y2)
pure_box :: Spec -> (Double, Double, Double, Double)
pure_box s = (x1, y1, x2, y2)
  where
    (x1, y1) = pure_pos s (sX1 s, sY1 s)
    (x2, y2) = pure_pos s (sX2 s, sY2 s)

-- Buang ruang kosong di hujung kiri dan kanan
pure_strip :: String -> String
pure_strip = dropWhileEnd isSpace . dropWhile isSpace

-- Buang kurungan: "[kursus]" -> "kursus". Selamat pada rentetan pendek
-- (tidak guna tail/init yang separa).
pure_unbracket :: String -> String
pure_unbracket = pure_strip . reverse . drop 1 . reverse . drop 1

-- Bulatkan ke bawah kepada 0.1
pure_floorTenth :: Double -> Double
pure_floorTenth x = fromIntegral (floor (x * 10) :: Int) / 10

-- ---- Ukuran dan susun atur ----

-- Saiz font ikut LEBAR. Args: lebarKotak(pt) saizMaks saizMin lebarDiukurPadaSaizMaks(pt)
pure_fitSize :: Double -> Double -> Double -> Double -> Double
pure_fitSize maxW sMax sMin w
  | w <= maxW = sMax
  | otherwise = max sMin (pure_floorTenth (sMax * maxW / w))

-- Susun atur dua baris dalam kotak.
-- Args: spec, ascent/saiz, descent/saiz, lebar nama pada saizMaks, lebar KP pada saizMaks (pt)
pure_layout :: Spec -> Double -> Double -> Double -> Double -> Layout
pure_layout s asc desc wN wK = Layout sN sK baseN baseK status
  where
    (bx1, by1, bx2, by2) = pure_box s
    boxW = bx2 - bx1
    boxH = by2 - by1
    gap  = pure_toPt (sGap s)
    lh   = asc + desc                       -- tinggi baris per 1 pt saiz
    need a b = lh * (a + b) + gap           -- tinggi blok dua baris

    -- 1) ikut lebar
    nW = pure_fitSize boxW (sNamaMax s) (sNamaMin s) wN
    kW = pure_fitSize boxW (sKpMax s) (sKpMin s) wK

    -- 2) ikut tinggi (kecilkan kedua-dua baris mengikut nisbah jika perlu)
    f = (boxH - gap) / (lh * (nW + kW))
    (sN, sK)
      | need nW kW <= boxH = (nW, kW)
      | otherwise = ( max (sNamaMin s) (pure_floorTenth (nW * f))
                    , max (sKpMin s)   (pure_floorTenth (kW * f)) )

    -- 3) letak: blok ditengahkan menegak dalam kotak
    blockH = need sN sK
    top    = by1 + (boxH - blockH) / 2
    baseN  = top + asc * sN
    baseK  = top + lh * sN + gap + asc * sK

    -- 4) semak
    eps      = 0.01
    widthOK  = wN * sN / sNamaMax s <= boxW + eps
            && wK * sK / sKpMax s   <= boxW + eps
    heightOK = blockH <= boxH + eps
    status
      | not (widthOK && heightOK)            = Terlebih
      | sN < sNamaMax s || sK < sKpMax s     = Dikecilkan
      | otherwise                            = Muat

-- ---- Semakan data CSV ----

-- Format No. KP baru: 6 digit - 2 digit - 4 digit (contoh 900101-14-5555).
-- Cara: tukar setiap digit kepada 'd', kemudian banding dengan bentuk yang dijangka.
--   pure_isKP "900101-14-5555" == True
--   pure_isKP "9001011455"     == False
pure_isKP :: String -> Bool
pure_isKP s = map cls s == "dddddd-dd-dddd"
  where
    cls c
      | isDigit c = 'd'
      | otherwise = c

-- Semak semua baris. Pulangkan (ralat, amaran).
--   RALAT  = sijil tidak boleh dijana (nama kosong, template tidak dikenali)
--   AMARAN = boleh dijana tetapi patut disemak (format No. KP, fail tiada data)
-- Args: senarai nama template dalam config, fail CSV. Baris 1 = tajuk, data bermula baris 2.
pure_checkRows :: [String] -> [Fail] -> ([String], [String])
pure_checkRows known files = (concatMap fst per, concatMap snd per)
  where
    per :: [([String], [String])]
    per =
      [ (errs f n p, warns f n p) | (f, ps) <- files, (n, p) <- zip [2 :: Int ..] ps ]
      ++
      -- fail kosong (0 bait) atau hanya ada tajuk: bukan ralat, tetapi patut dimaklumkan
      [ ([], [printf "%s: fail kosong atau tiada baris data (fail ini dilangkau)" f])
      | (f, ps) <- files, null ps ]

    errs :: FilePath -> Int -> Peserta -> [String]
    errs f n p =
      [ printf "%s baris %d: nama kosong" f n
      | null (pNama p) ]
      ++
      [ printf "%s baris %d (%s): template '%s' tiada atau dinyahaktifkan dalam config (yang ada: %s)"
          f n (pNama p) (pTemplate p) (intercalate ", " known)
      | pTemplate p `notElem` known ]

    warns :: FilePath -> Int -> Peserta -> [String]
    warns f n p =
      [ printf "%s baris %d (%s): No. KP '%s' bukan format 900101-14-5555" f n (pNama p) (pNoKP p)
      | not (pure_isKP (pNoKP p)) ]

-- ---- Laporan ----

-- Laporan semakan: jadual template + ralat + amaran + kesimpulan
pure_report :: [Spec] -> [Fail] -> [String] -> [String] -> String
pure_report specs files errs warns =
  unlines $
    [ "=== SEMAKAN ==="
    , "Config  : " ++ show (length specs) ++ " template aktif"
    , "Data    : " ++ unwords (map fst files)
    , ""
    , printf "%-14s %-13s %-22s %6s  %s" "Template" "Kertas (mm)" "Kotak x1,y1 - x2,y2" "Sijil" ""
    ]
      ++ map line specs
      ++ [""]
      ++ section "RALAT" errs
      ++ section "AMARAN" warns
      ++ [ conclusion ]
  where
    cnt s    = length [ () | (_, ps) <- files, p <- ps, pTemplate p == sName s ]
    maxCnt   = maximum (1 : map cnt specs)
    bar n    = replicate (n * 30 `div` maxCnt) '█'
    line :: Spec -> String
    line s =
      printf "%-14s %-13s %-22s %6d  %s"
        (sName s)
        (printf "%.1fx%.1f" (sPageW s) (sPageH s) :: String)
        (printf "%.1f,%.1f - %.1f,%.1f" (sX1 s) (sY1 s) (sX2 s) (sY2 s) :: String)
        (cnt s) (bar (cnt s))
    section :: String -> [String] -> [String]
    section _ [] = []
    section title xs =
      (title ++ " (" ++ show (length xs) ++ "):") : map ("  - " ++) xs ++ [""]
    conclusion :: String
    conclusion
      | not (null errs)  = printf "KESIMPULAN: ADA %d RALAT. Betulkan dahulu, sijil belum dijana." (length errs)
      | not (null warns) = printf "KESIMPULAN: boleh dijana, tetapi semak %d amaran di atas." (length warns)
      | otherwise        = "KESIMPULAN: semua OK."

-- Ringkasan selepas jana: carta bar status + senarai PDF + langkah seterusnya
pure_summary :: FilePath -> [Hasil] -> String
pure_summary outDir hs =
  unlines $
    [ "=== RINGKASAN ==="
    , row "Muat" Muat
    , row "Dikecilkan" Dikecilkan
    , row "TERLEBIH" Terlebih
    , ""
    ]
      ++ notable
      ++ [ "" | not (null notable) ]
      ++ [ "PDF dijana:" ]
      ++ [ printf "  %s  (%d sijil)" (outDir </> (t <.> "pdf")) n | (t, n) <- Map.toList perTpl ]
      ++ [ ""
         , pure_conclusion (count Terlebih) (count Dikecilkan) total
         , "LANGKAH SETERUSNYA: cetak setiap PDF dengan Actual size / 100%, satu jenis kertas pada satu masa."
         ]
  where
    total   = length hs
    count s = length (filter ((== s) . hStatus) hs)
    perTpl  = Map.fromListWith (+) [ (hTpl h, 1 :: Int) | h <- hs ]
    bar n   = replicate (if total == 0 then 0 else n * 30 `div` total) '█'
    row :: String -> Status -> String
    row label s = printf "%-12s %4d  %s" label (count s) (bar (count s))
    notable :: [String]
    notable =
      [ printf "  %-36s %-12s nama %5.1f pt, KP %5.1f pt  %s"
          (hNama h) (hTpl h) (hNamaSz h) (hKpSz h) (note (hStatus h))
      | h <- hs, hStatus h /= Muat ]
    note :: Status -> String
    note Dikecilkan = "(dikecilkan)"
    note Terlebih   = "(TERLEBIH walau saiz minimum - semak manual!)"
    note Muat       = ""

pure_conclusion :: Int -> Int -> Int -> String
pure_conclusion over shrunk total
  | over > 0   = printf "KESIMPULAN: %d/%d sijil MASIH terlebih pada saiz minimum. Besarkan kotak_mm atau kecilkan saiz min dalam config." over total
  | shrunk > 0 = printf "KESIMPULAN: semua %d sijil muat; %d dikecilkan automatik." total shrunk
  | otherwise  = printf "KESIMPULAN: semua %d sijil muat pada saiz maksimum." total

-- Panduan penuh (arahan `bantuan`)
pure_help :: String
pure_help = unlines
  [ "jana-sijil - cetak NAMA + NO. KP pada ruang kosong kertas sijil (kedudukan tepat, mm)"
  , ""
  , "ALIRAN KERJA"
  , "   data/*.csv --+"
  , "                +--> semak --> ujian --> jana --> output/<template>.pdf --> cetak"
  , "   config.txt --+"
  , ""
  , "ARAHAN"
  , "   jana-sijil semak CONFIG CSV...        semak config + data, TIDAK jana PDF (selamat)"
  , "   jana-sijil ujian CONFIG OUTDIR        PDF kalibrasi (grid mm + kotak biru), cetak atas kertas biasa"
  , "   jana-sijil jana  CONFIG OUTDIR CSV... jana sijil sebenar, satu PDF setiap template"
  , "   jana-sijil bantuan                    paparan ini"
  , ""
  , "CONTOH"
  , "   jana-sijil semak config.txt data/contoh.csv"
  , "   jana-sijil jana  config.txt output/ data/penghargaan.csv data/penyertaan.csv"
  , "   (dalam projek ini, `make semak` dan `make jana` melakukannya untuk anda)"
  , ""
  , "FORMAT CSV   (UTF-8, baris pertama = tajuk)"
  , "   nama,nokp,template"
  , "   AHMAD BIN ALI,900101-14-5555,kursus-a"
  , "   Satu CSV boleh mengandungi beberapa template. Setiap nama dalam lajur"
  , "   'template' mesti ada seksyen [nama] yang sama dalam config.txt."
  , "   Fail CSV kosong atau hanya ada tajuk: diberi amaran dan dilangkau."
  , ""
  , "CONFIG   (lihat komen di kepala config.txt untuk semua kunci)"
  , "   [global]  nilai lalai untuk semua template"
  , "   [nama]    kertas_mm = 297 210        lebar tinggi kertas (mm)"
  , "             kotak_mm  = 60 95 237 125  x1 y1 x2 y2 ruang kosong, dari SUDUT KIRI ATAS (mm)"
  , "             nama_saiz = 12 24          saiz font nama: min maks (pt)"
  , ""
  , "TIPS"
  , "   - Cetak dengan Actual size / 100%, bukan Fit to page."
  , "   - Semak nama font sah:  fc-list : family | sort -u"
  , "   - Cetakan anjak sama arah? Betulkan offset_mm dalam [global] (lihat README.md)."
  , "   - Jangan commit data/*.csv atau output/ ke git (mengandungi No. KP sebenar)."
  ]

-- ---- Pembacaan config (format seksyen) ----
type Section = (String, [(String, String)])

-- Kunci yang dibenarkan dalam config. Kunci lain ditolak (menangkap salah taip).
pure_knownKeys :: [String]
pure_knownKeys =
  [ "font", "tebal", "kertas_mm", "offset_mm", "kotak_mm"
  , "nama_saiz", "kp_saiz", "jarak_mm" ]

-- Pecahkan config kepada seksyen [nama] + pasangan kunci = nilai
pure_parseSections :: String -> Either String [Section]
pure_parseSections src = go Nothing [] (zip [1 :: Int ..] (lines src))
  where
    clean = pure_strip . takeWhile (/= '#')

    flush Nothing  acc = acc
    flush (Just s) acc = s : acc

    go cur acc [] = Right (reverse (flush cur acc))
    go cur acc ((n, raw) : rest) =
      case clean raw of
        "" -> go cur acc rest
        l | "[" `isPrefixOf` l && "]" `isSuffixOf` l ->
              go (Just (pure_unbracket l, [])) (flush cur acc) rest
          | '=' `elem` l, Just (name, kvs) <- cur ->
              let (k, v) = break (== '=') l
              in go (Just (name, kvs ++ [(pure_strip k, pure_strip (drop 1 v))])) acc rest
          | '=' `elem` l ->
              Left ("Baris " ++ show n ++ ": tetapan di luar seksyen [..]")
          | otherwise ->
              Left ("Baris " ++ show n ++ " tidak difahami: " ++ l)

pure_parseConfig :: String -> Either String [Spec]
pure_parseConfig src = do
  secs <- pure_parseSections src
  let (gs, ts) = partition ((== "global") . fst) secs
      gkv   = concatMap snd gs
      names = map fst ts
      dups  = [ n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names ]
      badKeys =
        [ "[" ++ sec ++ "] kunci tidak dikenali: '" ++ k ++ "'"
        | (sec, kvs) <- secs, (k, _) <- kvs, k `notElem` pure_knownKeys ]
  unless (null badKeys) $
    Left (intercalate "\n" badKeys ++ "\n(kunci yang sah disenaraikan di kepala config.txt)")
  unless (null dups) $ Left ("Template berganda dalam config: " ++ unwords dups)
  when (null ts) $
    Left "Tiada template aktif dalam config.txt (semua dinyahaktifkan dengan # ?)"
  mapM (pure_buildSpec gkv) ts

-- Gabung [global] + seksyen template: nilai template menggantikan [global]
pure_buildSpec :: [(String, String)] -> Section -> Either String Spec
pure_buildSpec gkv (name, tkv) = do
  font     <- look "font"
  bold     <- look "tebal" >>= pure_yesNo
  (pw, ph) <- pair "kertas_mm"
  (ox, oy) <- pair "offset_mm"
  (x1, y1, x2, y2) <- quad "kotak_mm"
  (nMin, nMax) <- pair "nama_saiz"
  (kMin, kMax) <- pair "kp_saiz"
  gap      <- single "jarak_mm"
  pure_validate (Spec name font bold pw ph ox oy x1 y1 x2 y2 nMin nMax kMin kMax gap)
  where
    err :: String -> Either String a
    err m = Left ("[" ++ name ++ "] " ++ m)

    pure_yesNo :: String -> Either String Bool
    pure_yesNo "ya"    = Right True
    pure_yesNo "tidak" = Right False
    pure_yesNo _       = err "'tebal' mesti 'ya' atau 'tidak'"

    look :: String -> Either String String
    look key = case lookup key tkv of
      Just v  -> Right v
      Nothing -> case lookup key gkv of
        Just v  -> Right v
        Nothing -> err ("tiada tetapan '" ++ key ++ "' (tidak juga dalam [global])")

    nums :: Int -> String -> Either String [Double]
    nums n key = do
      v <- look key
      case mapM readMaybe (words v) of
        Just xs | length xs == n -> Right xs
        _ -> err ("'" ++ key ++ "' mesti " ++ show n ++ " nombor, contoh: " ++ example key)

    example :: String -> String
    example "kotak_mm"  = "60 95 237 125"
    example "kertas_mm" = "297 210"
    example "offset_mm" = "0 0"
    example "jarak_mm"  = "3"
    example _           = "12 24"

    single :: String -> Either String Double
    single key = nums 1 key >>= \xs -> case xs of
      [a] -> Right a
      _   -> err "ralat dalaman"

    pair :: String -> Either String (Double, Double)
    pair key = nums 2 key >>= \xs -> case xs of
      [a, b] -> Right (a, b)
      _      -> err "ralat dalaman"

    quad :: String -> Either String (Double, Double, Double, Double)
    quad key = nums 4 key >>= \xs -> case xs of
      [a, b, c, d] -> Right (a, b, c, d)
      _            -> err "ralat dalaman"

pure_validate :: Spec -> Either String Spec
pure_validate s
  | sX2 s <= sX1 s || sY2 s <= sY1 s =
      bad "kotak_mm: x2 mesti > x1 dan y2 mesti > y1"
  | sX1 s < 0 || sY1 s < 0 || sX2 s > sPageW s || sY2 s > sPageH s =
      bad "kotak_mm keluar dari kertas (semak kertas_mm: landskap 297 210, potret 210 297)"
  | sNamaMin s <= 0 || sKpMin s <= 0 =
      bad "saiz min mesti lebih daripada 0"
  | sNamaMin s > sNamaMax s || sKpMin s > sKpMax s =
      bad "saiz min mesti <= saiz maks"
  | otherwise = Right s
  where
    bad m = Left ("[" ++ sName s ++ "] " ++ m)

-- ===================================================================
-- 4. FUNGSI IO / RENDER (ada kesan sampingan)
-- ===================================================================

io_fail :: String -> IO a
io_fail msg = hPutStrLn stderr ("RALAT: " ++ msg) >> exitFailure

-- Pastikan fail wujud; jika tidak, beri mesej yang jelas
io_requireFile :: String -> FilePath -> IO ()
io_requireFile what path = do
  ok <- doesFileExist path
  unless ok $ io_fail (what ++ " tidak dijumpai: " ++ path ++ "  (taip 'jana-sijil bantuan' untuk panduan)")

io_setFont :: Spec -> Render ()
io_setFont s =
  selectFontFace (sFont s) FontSlantNormal
    (if sBold s then FontWeightBold else FontWeightNormal)

-- Ukur lebar teks (pt) pada saiz tertentu
io_textWidth :: Double -> String -> Render Double
io_textWidth size txt = do
  setFontSize size
  ext <- textExtents txt
  pure (textExtentsXadvance ext)

-- (ascent, descent) per 1 pt saiz font, diukur pada saiz 100
io_fontRatios :: Render (Double, Double)
io_fontRatios = do
  setFontSize 100
  fe <- fontExtents
  pure (fontExtentsAscent fe / 100, fontExtentsDescent fe / 100)

-- Lukis teks dipusatkan pada x (pt), garis dasar y (pt)
io_drawCenteredPt :: Double -> Double -> Double -> String -> Render ()
io_drawCenteredPt cx y size txt = do
  w <- io_textWidth size txt
  moveTo (cx - w / 2) y
  showText txt

-- Satu sijil: nama + No. KP dalam kotak
io_renderSijil :: Spec -> Peserta -> Render Hasil
io_renderSijil s p = do
  setSourceRGB 0 0 0
  io_setFont s
  (asc, desc) <- io_fontRatios
  let nama = pNama p
      kp   = pNoKP p
  wN <- io_textWidth (sNamaMax s) nama
  wK <- io_textWidth (sKpMax s) kp
  let l = pure_layout s asc desc wN wK
      (bx1, _, bx2, _) = pure_box s
      cx = (bx1 + bx2) / 2
  io_drawCenteredPt cx (lNamaBase l) (lNamaSize l) nama
  io_drawCenteredPt cx (lKpBase l) (lKpSize l) kp
  pure (Hasil nama (sName s) (lNamaSize l) (lKpSize l) (lStatus l))

-- Satu PDF untuk satu template
io_generateOne :: FilePath -> Spec -> [Peserta] -> IO [Hasil]
io_generateOne outDir s ps =
  withPDFSurface (outDir </> (sName s <.> "pdf")) (pure_toPt (sPageW s)) (pure_toPt (sPageH s)) $ \surf ->
    renderWith surf $ forM ps $ \p -> do
      h <- io_renderSijil s p
      showPage
      pure h

io_loadConfig :: FilePath -> IO [Spec]
io_loadConfig path = do
  io_requireFile "Fail config" path
  readFile path >>= either io_fail pure . pure_parseConfig

-- Baca satu fail CSV.
--   - BOM UTF-8 (kadang-kadang ditambah oleh Calc) dibuang
--   - fail kosong / hanya ruang kosong dipulangkan sebagai 0 baris
--     (pure_checkRows akan beri amaran; cassava sendiri akan ranap pada 0 bait)
io_loadCsv :: FilePath -> IO Fail
io_loadCsv path = do
  io_requireFile "Fail CSV" path
  raw <- BL.readFile path
  let bytes = if BL.take 3 raw == BL.pack [0xEF, 0xBB, 0xBF] then BL.drop 3 raw else raw
      blank = BL.all (`elem` [9, 10, 13, 32]) bytes   -- tab, LF, CR, ruang
  if blank
    then pure (path, [])
    else case Csv.decodeByName bytes of
      Left err      -> io_fail (path ++ ": CSV tidak boleh dibaca: " ++ err
                                ++ "\n(pastikan baris pertama ialah nama,nokp,template dan fail ialah UTF-8)")
      Right (_, vs) -> pure (path, V.toList vs)

-- Muat config + CSV, semak, paparkan laporan. Berhenti jika ada ralat.
-- Dikongsi oleh arahan `semak` dan `jana`.
io_prepare :: FilePath -> [FilePath] -> IO ([Spec], [Fail])
io_prepare cfgPath csvPaths = do
  specs <- io_loadConfig cfgPath
  files <- forM csvPaths io_loadCsv
  let (errs0, warns) = pure_checkRows (map sName specs) files
      errs = errs0 ++ [ "tiada sebarang baris data dalam semua fail CSV" | all (null . snd) files ]
  putStr (pure_report specs files errs warns)
  unless (null errs) exitFailure
  pure (specs, files)

io_jana :: FilePath -> FilePath -> [FilePath] -> IO ()
io_jana cfgPath outDir csvPaths = do
  when (null csvPaths) $
    io_fail "tiada fail CSV diberi. Contoh: jana-sijil jana config.txt output/ data/contoh.csv"
  (specs, files) <- io_prepare cfgPath csvPaths
  let byTpl = Map.fromListWith (flip (++))
                [ (pTemplate p, [p]) | (_, ps) <- files, p <- ps ]
  createDirectoryIfMissing True outDir
  putStrLn ""
  hasil <- fmap concat $ forM specs $ \s ->
    case Map.lookup (sName s) byTpl of
      Nothing -> pure []   -- template aktif tetapi tiada data: tiada PDF
      Just ps -> io_generateOne outDir s ps
  putStr (pure_summary outDir hasil)

-- ---- Mod ujian / kalibrasi ----

-- Garisan antara dua titik (mm), offset dikenakan
io_seg :: Spec -> (Double, Double) -> (Double, Double) -> Render ()
io_seg s a b = do
  let (x1, y1) = pure_pos s a
      (x2, y2) = pure_pos s b
  moveTo x1 y1
  lineTo x2 y2
  stroke

io_drawGrid :: Spec -> Render ()
io_drawGrid s = do
  setSourceRGB 0.7 0.7 0.7
  setFontSize 6
  let xs = takeWhile (<= sPageW s) [0, 10 ..]
      ys = takeWhile (<= sPageH s) [0, 10 ..]
      major v = (round v :: Int) `mod` 50 == 0
      labelAt pos str = do
        let (px, py) = pure_pos s pos
        moveTo px py
        showText str
  forM_ xs $ \x -> do
    setLineWidth (if major x then 0.6 else 0.2)
    io_seg s (x, 0) (x, sPageH s)
    when (major x) $ labelAt (x + 0.5, 4) (show (round x :: Int))
  forM_ ys $ \y -> do
    setLineWidth (if major y then 0.6 else 0.2)
    io_seg s (0, y) (sPageW s, y)
    when (major y) $ labelAt (0.5, y - 0.5) (show (round y :: Int))

-- Segi empat biru = kotak ruang kosong
io_drawBox :: Spec -> Render ()
io_drawBox s = do
  setSourceRGB 0 0 0.8
  setLineWidth 0.8
  io_seg s (sX1 s, sY1 s) (sX2 s, sY1 s)
  io_seg s (sX2 s, sY1 s) (sX2 s, sY2 s)
  io_seg s (sX2 s, sY2 s) (sX1 s, sY2 s)
  io_seg s (sX1 s, sY2 s) (sX1 s, sY1 s)

-- Dua contoh: nama sangat panjang (kes terburuk) dan nama pendek (kes ideal)
pesertaPanjang, pesertaPendek :: Peserta
pesertaPanjang = Peserta "MUHAMMAD NAJIB BIN ABDUL RAHMAN AL-HAJ" "900101-14-5555" "ujian"
pesertaPendek  = Peserta "AHMAD BIN ALI" "900101-14-5555" "ujian"

io_renderUjian :: Spec -> Peserta -> Render ()
io_renderUjian s p = do
  io_setFont s
  io_drawGrid s
  io_drawBox s
  setSourceRGB 0 0 0
  setFontSize 10
  let (tx, ty) = pure_pos s (5, 8)
  moveTo tx ty
  showText ("Template: " ++ sName s)
  _ <- io_renderSijil s p
  pure ()

io_ujian :: FilePath -> FilePath -> IO ()
io_ujian cfgPath outDir = do
  specs <- io_loadConfig cfgPath
  createDirectoryIfMissing True outDir
  forM_ specs $ \s -> do
    let out = outDir </> (("ujian-" ++ sName s) <.> "pdf")
    withPDFSurface out (pure_toPt (sPageW s)) (pure_toPt (sPageH s)) $ \surf ->
      renderWith surf $ forM_ [pesertaPanjang, pesertaPendek] $ \p -> do
        io_renderUjian s p
        showPage
    putStrLn ("PDF ujian dijana: " ++ out ++ " (2 halaman: nama panjang, nama pendek)")
  putStrLn "LANGKAH SETERUSNYA: cetak atas kertas biasa (Actual size 100%), tindih pada sijil, halakan ke cahaya."

-- ===================================================================
-- 5. TITIK MASUK
-- ===================================================================

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    []                          -> putStr pure_help
    [a] | a `elem` helpWords    -> putStr pure_help
    ("semak" : cfg : csvs@(_:_)) -> void (io_prepare cfg csvs)
    ["ujian", cfg, out]         -> io_ujian cfg out
    ("jana" : cfg : out : csvs) -> io_jana cfg out csvs
    _ -> io_fail ("arahan tidak difahami: " ++ unwords args
                  ++ "\nTaip 'jana-sijil bantuan' untuk panduan.")
  where
    helpWords = ["bantuan", "help", "-h", "--help"]
