{-
================================================================================
 jana-sijil  -  cetak nama + No. KP pada kertas sijil (kedudukan tepat, mm)
================================================================================

 PETA FAIL INI (atas ke bawah)
   1. IMPORT          dari pustaka mana setiap komponen datang
   2. JENIS DATA      Spec (satu template), ColMap/CsvCfg (nama lajur CSV), Fail, Peserta, Layout, Hasil
   3. FUNGSI TULEN    pure_*  tiada kesan sampingan (kira, semak, format teks)
        - ukuran & susun atur  : pure_layout, pure_fitSize
        - semakan data         : pure_checkRows
        - laporan              : pure_report, pure_summary, pure_help
        - pembacaan config     : pure_parseConfig, pure_boxFrom, pure_colsFor dan pembantunya
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

 NOMBOR PENGENALAN (nokp)
   Format TIDAK disemak: bukan warganegara dan dokumen lain mempunyai format
   berlainan. Program hanya memastikan teks itu muat pada sijil.

 PERATURAN SAIZ FONT
   Saiz font No. KP TIDAK PERNAH lebih besar daripada saiz font nama.
   Peraturan ini dikira dalam pure_layout dan kekal walaupun nama dikecilkan
   (ikut lebar) atau kedua-dua baris dikecilkan (ikut tinggi).
   Peraturan ini diutamakan berbanding kp_saiz min.

 KOTAK (dua cara menulisnya dalam config, dalam SATU seksyen)
   A. kotak_mm = x1 y1 x2 y2
   B. kombinasi (keempat-empat diperlukan):
        kotakHorizontalCenterPoint_mm  titik tengah mendatar
        kotakWidth_mm                  lebar kotak
        kotakPointAtas_mm              y atas
        kotakPointBawah_mm             y bawah
      => x1 = tengah - lebar/2, x2 = tengah + lebar/2
   Jika kedua-duanya ditulis dalam seksyen yang sama, KOMBINASI DIUTAMAKAN.
   Jika seksyen template tiada kotak langsung, kotak diwarisi daripada [global]
   (peraturan sama). Kombinasi tidak dicampur antara seksyen.

 NAMA LAJUR CSV (tiga peringkat; yang lebih khusus menang, KUNCI demi kunci)
   [csv nama-fail.csv]  pemetaan untuk SATU fail (padan nama fail sahaja, tanpa folder)
   [csv]                pemetaan lalai untuk fail yang tiada seksyen sendiri
   (tiada)              nama lalai: nama, nokp, template
   Setiap seksyen boleh menulis kunci: nama, nokp, template (nilai = tajuk lajur CSV).
-}
module Main (main) where

-- base: forM, forM_, unless, when, void (gelung dan syarat dalam monad)
import Control.Monad (forM, forM_, unless, void, when)
-- bytestring: ByteString lazy (baca fail CSV mentah; all/take/drop/pack untuk BOM dan fail kosong)
import qualified Data.ByteString.Lazy as BL
-- cassava: decodeByName, NamedRecord, Parser, runParser, (.:) untuk baca lajur CSV ikut nama
import qualified Data.Csv as Csv
-- base: isSpace (buang ruang kosong)
import Data.Char (isSpace)
-- base: dropWhileEnd, intercalate, isPrefixOf, isSuffixOf
import Data.List (dropWhileEnd, intercalate, isPrefixOf, isSuffixOf)
-- containers: Map (kumpul peserta ikut template, kira bilangan)
import qualified Data.Map.Strict as Map
-- base: fromMaybe (nilai lalai bila kunci tiada)
import Data.Maybe (fromMaybe)
-- text: Text (medan CSV UTF-8), pack/unpack (tukar dengan String)
import qualified Data.Text as T
-- text: encodeUtf8, decodeUtf8With (nama lajur UTF-8 <-> String)
import qualified Data.Text.Encoding as TE
-- text: lenientDecode (jangan ranap jika ada bait UTF-8 rosak dalam tajuk)
import Data.Text.Encoding.Error (lenientDecode)
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
-- filepath: (</>) gabung laluan, (<.>) tambah sambungan, takeFileName (nama fail tanpa folder)
import System.FilePath (takeFileName, (<.>), (</>))
-- base: hPutStrLn, stderr (cetak ralat)
import System.IO (hPutStrLn, stderr)
-- base: printf (format jadual)
import Text.Printf (printf)
-- base: readMaybe (tukar teks ke nombor tanpa crash)
import Text.Read (readMaybe)

-- ===================================================================
-- 2. JENIS DATA
-- ===================================================================

-- Dari mana kotak sesuatu template diambil (dipaparkan dalam laporan semakan)
data SumberKotak
  = KotakMm            -- kotak_mm
  | Kombinasi          -- kombinasi 4 kunci
  | KombinasiDanKotak  -- kedua-duanya ditulis; kombinasi dipakai, kotak_mm diabaikan
  deriving (Eq)

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
  , sKpMax   :: Double  -- saiz PILIHAN No. KP (diturunkan jika nama lebih kecil)
  , sGap     :: Double  -- jarak antara baris nama dan No. KP
  , sBoxSrc  :: SumberKotak
  }

-- Nama lajur dalam SATU fail CSV (selepas pemetaan [csv] diselesaikan)
data ColMap = ColMap
  { cNama     :: String
  , cNoKP     :: String
  , cTemplate :: String
  }

-- Semua pemetaan lajur dari config:
--   ccDefault = kunci dalam seksyen [csv]
--   ccFiles   = (nama fail, kunci) untuk setiap seksyen [csv nama-fail.csv]
data CsvCfg = CsvCfg
  { ccDefault :: [(String, String)]
  , ccFiles   :: [(String, [(String, String)])]
  }

-- Satu baris dari CSV. Semua medan sudah dibuang ruang kosong di hujung.
data Peserta = Peserta
  { pNama     :: String
  , pNoKP     :: String
  , pTemplate :: String
  }

-- Satu fail CSV yang sudah dibaca
data Fail = Fail
  { fPath :: FilePath   -- laluan seperti diberi pada baris arahan (untuk mesej)
  , fRows :: [Peserta]
  , fMap  :: String     -- pemetaan lajur yang digunakan (untuk laporan)
  }

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
--
-- PERATURAN: saiz No. KP TIDAK PERNAH lebih besar daripada saiz nama.
--   Peraturan ini diutamakan berbanding kp_saiz min.
--   Lebar teks berskala linear dengan saiz, jadi lebar pada saiz s ialah w * s / saizMaks.
pure_layout :: Spec -> Double -> Double -> Double -> Double -> Layout
pure_layout s asc desc wN wK = Layout sN sK baseN baseK status
  where
    (bx1, by1, bx2, by2) = pure_box s
    boxW = bx2 - bx1
    boxH = by2 - by1
    gap  = pure_toPt (sGap s)
    lh   = asc + desc                       -- tinggi baris per 1 pt saiz
    need a b = lh * (a + b) + gap           -- tinggi blok dua baris

    -- 1) ikut LEBAR
    nFit = pure_fitSize boxW (sNamaMax s) (sNamaMin s) wN   -- saiz nama yang muat
    kFit = pure_fitSize boxW (sKpMax s) (sKpMin s) wK       -- saiz KP pilihan yang muat

    -- 2) PERATURAN: KP tidak melebihi nama
    nR = nFit
    kR = min kFit nFit

    -- 3) ikut TINGGI (kecilkan kedua-dua baris mengikut nisbah jika perlu)
    f = (boxH - gap) / (lh * (nR + kR))
    (sN, sK1)
      | need nR kR <= boxH = (nR, kR)
      | otherwise = ( max (sNamaMin s) (pure_floorTenth (nR * f))
                    , max (sKpMin s)   (pure_floorTenth (kR * f)) )
    -- peraturan dikekalkan walaupun had min KP lebih besar daripada saiz nama
    sK = min sK1 sN

    -- 4) letak: blok ditengahkan menegak dalam kotak
    blockH = need sN sK
    top    = by1 + (boxH - blockH) / 2
    baseN  = top + asc * sN
    baseK  = top + lh * sN + gap + asc * sK

    -- 5) semak
    eps      = 0.01
    widthOK  = wN * sN / sNamaMax s <= boxW + eps
            && wK * sK / sKpMax s   <= boxW + eps
    heightOK = blockH <= boxH + eps
    status
      | not (widthOK && heightOK)            = Terlebih
      | sN < sNamaMax s || sK < sKpMax s     = Dikecilkan
      | otherwise                            = Muat

-- ---- Semakan data CSV ----

-- Semak semua baris. Pulangkan (ralat, amaran).
--   RALAT  = sijil tidak boleh dijana (nama kosong, template tidak dikenali)
--   AMARAN = boleh dijana tetapi patut disemak (nokp kosong, fail tiada data)
-- Format nokp TIDAK disemak (bukan warganegara dsb. guna format berlainan).
-- Args: senarai nama template dalam config, fail CSV. Baris 1 = tajuk, data bermula baris 2.
pure_checkRows :: [String] -> [Fail] -> ([String], [String])
pure_checkRows known files = (concatMap fst per, concatMap snd per)
  where
    per :: [([String], [String])]
    per =
      [ (errs f n p, warns f n p) | Fail f ps _ <- files, (n, p) <- zip [2 :: Int ..] ps ]
      ++
      -- fail kosong (0 bait) atau hanya ada tajuk: bukan ralat, tetapi patut dimaklumkan
      [ ([], [printf "%s: fail kosong atau tiada baris data (fail ini dilangkau)" f])
      | Fail f ps _ <- files, null ps ]

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
      [ printf "%s baris %d (%s): nokp kosong" f n (pNama p)
      | null (pNoKP p) ]

-- ---- Laporan ----

pure_srcLabel :: SumberKotak -> String
pure_srcLabel KotakMm           = "kotak_mm"
pure_srcLabel Kombinasi         = "kombinasi"
pure_srcLabel KombinasiDanKotak = "kombinasi*"

-- Laporan semakan: fail data + pemetaan lajur, jadual template, ralat, amaran, kesimpulan
-- Args: spesifikasi, fail CSV, pemetaan [csv ...] yang tidak digunakan, ralat, amaran
pure_report :: [Spec] -> [Fail] -> [String] -> [String] -> [String] -> String
pure_report specs files unusedMaps errs warns =
  unlines $
    [ "=== SEMAKAN ==="
    , "Config  : " ++ show (length specs) ++ " template aktif"
    , "Data    :"
    ]
      ++ [ printf "  %-32s %5d baris   pemetaan lajur: %s" (fPath f) (length (fRows f)) (fMap f)
         | f <- files ]
      ++ [ "  (pemetaan tidak digunakan, tiada fail CSV sepadan: " ++ intercalate ", " unusedMaps ++ ")"
         | not (null unusedMaps) ]
      ++
    [ ""
    , printf "%-14s %-13s %-27s %-11s %6s" "Template" "Kertas (mm)" "Kotak x1,y1 - x2,y2" "Sumber" "Sijil"
    ]
      ++ map line specs
      ++ [ "* kotak_mm juga ditulis dalam seksyen itu tetapi DIABAIKAN (kombinasi diutamakan)."
         | any ((== KombinasiDanKotak) . sBoxSrc) specs ]
      ++ [""]
      ++ section "RALAT" errs
      ++ section "AMARAN" warns
      ++ [ conclusion ]
  where
    cnt s    = length [ () | Fail _ ps _ <- files, p <- ps, pTemplate p == sName s ]
    maxCnt   = maximum (1 : map cnt specs)
    bar n    = replicate (n * 30 `div` maxCnt) '█'
    line :: Spec -> String
    line s =
      printf "%-14s %-13s %-27s %-11s %6d  %s"
        (sName s)
        (printf "%.1fx%.1f" (sPageW s) (sPageH s) :: String)
        (printf "%.1f,%.1f - %.1f,%.1f" (sX1 s) (sY1 s) (sX2 s) (sY2 s) :: String)
        (pure_srcLabel (sBoxSrc s))
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
  | over > 0   = printf "KESIMPULAN: %d/%d sijil MASIH terlebih pada saiz minimum. Besarkan kotak atau kecilkan saiz min dalam config." over total
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
  , "   nama,nokp,template        (tajuk lain boleh dipetakan dalam [csv] config.txt;"
  , "   AHMAD BIN ALI,900101-14-5555,kursus-a   lajur tambahan lain diabaikan)"
  , "   nokp: apa-apa format diterima (No. KP, nombor pengenalan bukan warganegara, dsb.)."
  , "         Amaran hanya jika kosong."
  , "   Satu CSV boleh mengandungi beberapa template. Setiap nama dalam lajur"
  , "   'template' mesti ada seksyen [nama] yang sama dalam config.txt."
  , "   Fail CSV kosong atau hanya ada tajuk: diberi amaran dan dilangkau."
  , ""
  , "CONFIG   (lihat komen di kepala config.txt untuk semua kunci)"
  , "   [global]  nilai lalai untuk semua template"
  , "   [csv]     nama = NAMA GURU   (pemetaan lajur CSV lalai; pilihan)"
  , "   [csv fail.csv]  pemetaan untuk SATU fail sahaja (nama fail tanpa folder);"
  , "             menang atas [csv], kunci demi kunci. Kunci: nama, nokp, template"
  , "   [nama]    kertas_mm = 297 210        lebar tinggi kertas (mm)"
  , "             kotak_mm  = 60 95 237 125  x1 y1 x2 y2 ruang kosong, dari SUDUT KIRI ATAS (mm)"
  , "             ATAU kombinasi: kotakHorizontalCenterPoint_mm, kotakWidth_mm,"
  , "                             kotakPointAtas_mm, kotakPointBawah_mm  (kombinasi diutamakan)"
  , "             nama_saiz = 12 24          saiz font nama: min maks (pt)"
  , "             kp_saiz   = 10 16          saiz font No. KP: min maks (pt)"
  , ""
  , "SAIZ FONT"
  , "   - Nama panjang dikecilkan automatik supaya muat dalam kotak."
  , "   - Saiz No. KP TIDAK PERNAH lebih besar daripada saiz nama (diturunkan automatik)."
  , "     Peraturan ini diutamakan berbanding kp_saiz min."
  , ""
  , "TIPS"
  , "   - Cetak dengan Actual size / 100%, bukan Fit to page."
  , "   - Semak nama font sah:  fc-list : family | sort -u"
  , "   - Cetakan anjak sama arah? Betulkan offset_mm dalam [global] (lihat README.md)."
  , "   - Jangan commit data/*.csv atau output/ ke git (mengandungi No. KP sebenar)."
  ]

-- ---- Pembacaan config (format seksyen) ----
type Section = (String, [(String, String)])

-- Empat kunci kombinasi kotak (semua diperlukan jika kombinasi digunakan)
pure_comboKeys :: [String]
pure_comboKeys =
  [ "kotakHorizontalCenterPoint_mm", "kotakWidth_mm"
  , "kotakPointAtas_mm", "kotakPointBawah_mm" ]

-- Kunci yang dibenarkan dalam seksyen [global] dan seksyen template.
-- Kunci lain ditolak (menangkap salah taip).
pure_knownKeys :: [String]
pure_knownKeys =
  [ "font", "tebal", "kertas_mm", "offset_mm", "kotak_mm"
  , "nama_saiz", "kp_saiz", "jarak_mm" ] ++ pure_comboKeys

-- Kunci yang dibenarkan dalam seksyen [csv] dan [csv nama-fail.csv]
pure_csvKeys :: [String]
pure_csvKeys = ["nama", "nokp", "template"]

-- Adakah nama seksyen ini seksyen pemetaan CSV? ("csv" atau "csv <nama fail>")
pure_isCsvSection :: String -> Bool
pure_isCsvSection n = n == "csv" || "csv " `isPrefixOf` n

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

-- Config penuh: senarai template + pemetaan lajur CSV
pure_parseConfig :: String -> Either String ([Spec], CsvCfg)
pure_parseConfig src = do
  secs <- pure_parseSections src
  let isGlobal = (== "global") . fst
      gs = filter isGlobal secs
      ts = filter (\s -> not (isGlobal s || pure_isCsvSection (fst s))) secs
      gkv   = concatMap snd gs
      names = map fst ts
      dups  = [ n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names ]
      badKeys =
        [ "[" ++ sec ++ "] kunci tidak dikenali: '" ++ k ++ "'"
        | (sec, kvs) <- gs ++ ts, (k, _) <- kvs, k `notElem` pure_knownKeys ]
  unless (null badKeys) $
    Left (intercalate "\n" badKeys ++ "\n(kunci yang sah disenaraikan di kepala config.txt)")
  unless (null dups) $ Left ("Template berganda dalam config: " ++ unwords dups
    ++ "\n(satu nama template = SATU seksyen; letak # pada seksyen yang tidak digunakan)")
  when (null ts) $
    Left "Tiada template aktif dalam config.txt (semua dinyahaktifkan dengan # ?)"
  csvCfg <- pure_buildCsvCfg secs
  specs  <- mapM (pure_buildSpec gkv) ts
  pure (specs, csvCfg)

-- Pemetaan lajur CSV dari seksyen [csv] dan [csv nama-fail.csv].
-- Kunci tiada = peringkat seterusnya (lihat pure_colsFor). Semua kunci disemak.
pure_buildCsvCfg :: [Section] -> Either String CsvCfg
pure_buildCsvCfg secs = do
  let csvSecs  = [ (n, kv) | (n, kv) <- secs, pure_isCsvSection n ]
      defaults = concat [ kv | (n, kv) <- csvSecs, n == "csv" ]
      files    = [ (pure_strip (drop 4 n), kv) | (n, kv) <- csvSecs, "csv " `isPrefixOf` n ]
      fnames   = map (takeFileName . fst) files
      dups     = [ f | (i, f) <- zip [0 :: Int ..] fnames, f `elem` take i fnames ]
      bad      = [ "[" ++ n ++ "] kunci tidak dikenali: '" ++ k ++ "'  (yang sah: "
                   ++ unwords pure_csvKeys ++ ")"
                 | (n, kv) <- csvSecs, (k, _) <- kv, k `notElem` pure_csvKeys ]
  unless (null bad) $ Left (intercalate "\n" bad)
  unless (null dups) $
    Left ("Seksyen [csv ...] berganda untuk fail: " ++ unwords dups
          ++ "\n(satu fail = SATU seksyen; letak # pada yang tidak digunakan)")
  pure (CsvCfg defaults files)

-- Pemetaan lajur untuk SATU fail CSV. Pulangkan (pemetaan, label untuk laporan).
--   Setiap kunci: [csv nama-fail.csv] dahulu, kemudian [csv], kemudian nama lalai.
--   Fail dipadan dengan NAMA FAIL sahaja (folder diabaikan).
pure_colsFor :: CsvCfg -> FilePath -> (ColMap, String)
pure_colsFor cc path = (ColMap (pick "nama") (pick "nokp") (pick "template"), label)
  where
    fileKv = lookup (takeFileName path) [ (takeFileName f, kv) | (f, kv) <- ccFiles cc ]
    pick k = case fileKv >>= lookup k of
      Just v  -> v
      Nothing -> fromMaybe k (lookup k (ccDefault cc))
    label = case fileKv of
      Just _  -> "[csv " ++ takeFileName path ++ "]"
      Nothing | null (ccDefault cc) -> "lalai (nama, nokp, template)"
              | otherwise           -> "[csv]"

-- Seksyen [csv nama-fail.csv] yang tiada fail CSV sepadan (biasanya salah eja nama fail)
pure_unusedMaps :: CsvCfg -> [FilePath] -> [String]
pure_unusedMaps cc paths =
  [ "[csv " ++ f ++ "]" | (f, _) <- ccFiles cc, takeFileName f `notElem` map takeFileName paths ]

-- Cari kotak dalam SATU senarai kunci (satu seksyen sahaja).
--   Nothing          = seksyen ini tidak menulis kotak langsung
--   Just (Left e)    = ditulis tetapi salah (mesej e)
--   Just (Right ..)  = (x1, y1, x2, y2, sumber)
-- PERATURAN: jika mana-mana kunci kombinasi ditulis, KOMBINASI digunakan dan
-- kotak_mm diabaikan. Jika tiada, guna kotak_mm.
pure_boxFrom :: [(String, String)]
             -> Maybe (Either String (Double, Double, Double, Double, SumberKotak))
pure_boxFrom kv
  | any hasKey pure_comboKeys = Just combo
  | hasKey "kotak_mm"         = Just plain
  | otherwise                 = Nothing
  where
    hasKey k = k `elem` map fst kv

    missing = [ k | k <- pure_comboKeys, not (hasKey k) ]

    num :: String -> Either String Double
    num k = case lookup k kv >>= readMaybe of
      Just v  -> Right v
      Nothing -> Left ("'" ++ k ++ "' mesti satu nombor, contoh: 114")

    combo :: Either String (Double, Double, Double, Double, SumberKotak)
    combo
      | not (null missing) =
          Left ("kombinasi kotak tidak lengkap, tiada: " ++ intercalate ", " missing
                ++ " (keempat-empat diperlukan, atau guna kotak_mm sahaja)")
      | otherwise = do
          c <- num "kotakHorizontalCenterPoint_mm"
          w <- num "kotakWidth_mm"
          a <- num "kotakPointAtas_mm"
          b <- num "kotakPointBawah_mm"
          pure ( c - w / 2, a, c + w / 2, b
               , if hasKey "kotak_mm" then KombinasiDanKotak else Kombinasi )

    plain :: Either String (Double, Double, Double, Double, SumberKotak)
    plain = case lookup "kotak_mm" kv >>= mapM readMaybe . words of
      Just [a, b, c, d] -> Right (a, b, c, d, KotakMm)
      _                 -> Left "'kotak_mm' mesti 4 nombor, contoh: 60 95 237 125"

-- Gabung [global] + seksyen template: nilai template menggantikan [global]
pure_buildSpec :: [(String, String)] -> Section -> Either String Spec
pure_buildSpec gkv (name, tkv) = do
  font     <- look "font"
  bold     <- look "tebal" >>= pure_yesNo
  (pw, ph) <- pair "kertas_mm"
  (ox, oy) <- pair "offset_mm"
  (x1, y1, x2, y2, src) <- box
  (nMin, nMax) <- pair "nama_saiz"
  (kMin, kMax) <- pair "kp_saiz"
  gap      <- single "jarak_mm"
  pure_validate (Spec name font bold pw ph ox oy x1 y1 x2 y2 nMin nMax kMin kMax gap src)
  where
    err :: String -> Either String a
    err m = Left ("[" ++ name ++ "] " ++ m)

    pure_yesNo :: String -> Either String Bool
    pure_yesNo "ya"    = Right True
    pure_yesNo "tidak" = Right False
    pure_yesNo _       = err "'tebal' mesti 'ya' atau 'tidak'"

    -- kotak: seksyen template dahulu; jika tiada kotak di situ, warisi dari [global]
    box :: Either String (Double, Double, Double, Double, SumberKotak)
    box = case pure_boxFrom tkv of
      Just r  -> either err Right r
      Nothing -> case pure_boxFrom gkv of
        Just r  -> either err Right r
        Nothing -> err ("tiada kotak: tulis kotak_mm ATAU kombinasi ("
                        ++ intercalate ", " pure_comboKeys ++ ")")

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

pure_validate :: Spec -> Either String Spec
pure_validate s
  | sX2 s <= sX1 s || sY2 s <= sY1 s =
      bad "kotak: x2 mesti > x1 dan y2 mesti > y1 (kombinasi: kotakWidth_mm > 0 dan Bawah > Atas)"
  | sX1 s < 0 || sY1 s < 0 || sX2 s > sPageW s || sY2 s > sPageH s =
      bad "kotak keluar dari kertas (semak kertas_mm: landskap 297 210, potret 210 297)"
  | sNamaMin s <= 0 || sKpMin s <= 0 =
      bad "saiz min mesti lebih daripada 0"
  | sNamaMin s > sNamaMax s || sKpMin s > sKpMax s =
      bad "saiz min mesti <= saiz maks"
  | otherwise = Right s
  where
    bad m = Left ("[" ++ sName s ++ "] " ++ m)

-- Tukar satu baris CSV (NamedRecord) kepada Peserta mengikut pemetaan lajur
pure_toPeserta :: ColMap -> Csv.NamedRecord -> Either String Peserta
pure_toPeserta cols r =
  Csv.runParser (Peserta <$> field (cNama cols) <*> field (cNoKP cols) <*> field (cTemplate cols))
  where
    field :: String -> Csv.Parser String
    field name = do
      t <- r Csv..: TE.encodeUtf8 (T.pack name)
      pure (pure_strip (T.unpack (t :: T.Text)))

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

io_loadConfig :: FilePath -> IO ([Spec], CsvCfg)
io_loadConfig path = do
  io_requireFile "Fail config" path
  readFile path >>= either io_fail pure . pure_parseConfig

-- Baca satu fail CSV.
--   - BOM UTF-8 (kadang-kadang ditambah oleh Calc) dibuang
--   - fail kosong / hanya ruang kosong dipulangkan sebagai 0 baris
--     (pure_checkRows akan beri amaran; cassava sendiri akan ranap pada 0 bait)
--   - lajur yang diperlukan (ikut pemetaan) mesti ada dalam tajuk; lajur lain diabaikan
-- Args: (pemetaan lajur, label pemetaan) untuk fail ini, seksyen [csv ...] yang tidak
--       sepadan dengan mana-mana fail (petunjuk salah eja), laluan fail
io_loadCsv :: (ColMap, String) -> [String] -> FilePath -> IO Fail
io_loadCsv (cols, mapLabel) unused path = do
  io_requireFile "Fail CSV" path
  raw <- BL.readFile path
  let bytes = if BL.take 3 raw == BL.pack [0xEF, 0xBB, 0xBF] then BL.drop 3 raw else raw
      blank = BL.all (`elem` [9, 10, 13, 32]) bytes   -- tab, LF, CR, ruang
  if blank
    then pure (Fail path [] mapLabel)
    else case Csv.decodeByName bytes of
      Left err -> io_fail (path ++ ": CSV tidak boleh dibaca: " ++ err
                           ++ "\n(pastikan baris pertama ialah tajuk lajur dan fail ialah UTF-8)")
      Right (hdr, vs) -> do
        let have    = [ T.unpack (TE.decodeUtf8With lenientDecode h) | h <- V.toList hdr ]
            need    = [cNama cols, cNoKP cols, cTemplate cols]
            missing = [ c | c <- need, c `notElem` have ]
        unless (null missing) $
          io_fail (path ++ ": lajur CSV tiada: " ++ intercalate ", " [ "'" ++ c ++ "'" | c <- missing ]
                   ++ "\n  lajur yang ada : " ++ intercalate ", " have
                   ++ "\n  pemetaan guna  : " ++ mapLabel
                   ++ (if null unused then ""
                       else "\n  petunjuk       : seksyen ini tiada fail sepadan (semak ejaan nama fail): "
                            ++ intercalate ", " unused)
                   ++ "\n  (nama lajur dipetakan dalam seksyen [csv " ++ takeFileName path
                   ++ "] atau [csv] dalam config.txt)")
        case mapM (pure_toPeserta cols) (V.toList vs) of
          Left err -> io_fail (path ++ ": baris CSV tidak boleh dibaca: " ++ err)
          Right ps -> pure (Fail path ps mapLabel)

-- Muat config + CSV, semak, paparkan laporan. Berhenti jika ada ralat.
-- Dikongsi oleh arahan `semak` dan `jana`.
io_prepare :: FilePath -> [FilePath] -> IO ([Spec], [Fail])
io_prepare cfgPath csvPaths = do
  (specs, csvCfg) <- io_loadConfig cfgPath
  let unusedMaps = pure_unusedMaps csvCfg csvPaths
  files <- forM csvPaths $ \p -> io_loadCsv (pure_colsFor csvCfg p) unusedMaps p
  let (errs0, warns) = pure_checkRows (map sName specs) files
      errs = errs0 ++ [ "tiada sebarang baris data dalam semua fail CSV" | all (null . fRows) files ]
  putStr (pure_report specs files unusedMaps errs warns)
  unless (null errs) exitFailure
  pure (specs, files)

io_jana :: FilePath -> FilePath -> [FilePath] -> IO ()
io_jana cfgPath outDir csvPaths = do
  when (null csvPaths) $
    io_fail "tiada fail CSV diberi. Contoh: jana-sijil jana config.txt output/ data/contoh.csv"
  (specs, files) <- io_prepare cfgPath csvPaths
  let byTpl = Map.fromListWith (flip (++))
                [ (pTemplate p, [p]) | Fail _ ps _ <- files, p <- ps ]
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
  showText ("Template: " ++ sName s ++ "   kotak: " ++ pure_srcLabel (sBoxSrc s))
  _ <- io_renderSijil s p
  pure ()

io_ujian :: FilePath -> FilePath -> IO ()
io_ujian cfgPath outDir = do
  (specs, _) <- io_loadConfig cfgPath
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
