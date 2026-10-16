# hsijil: cetak nama + No. KP pada kertas sijil

Program Haskell yang mencetak **nama** dan **No. KP** pada ruang kosong
kertas sijil yang sudah bercetak, pada kedudukan tepat (mm), dengan saiz
font dilaraskan automatik untuk nama panjang.

```
 data/*.csv ──┐
              ├─► jana-sijil ─► output/<template>.pdf ─► cetak
 config.txt ──┘
```

## Mula pantas (operator)

```
$ cd hsijil          # direnv menyediakan persekitaran secara automatik
$ make bina          # sekali sahaja
$ make semak         # semak data. Baca RALAT/AMARAN jika ada
$ make jana          # PDF ada dalam output/
```

Cetak setiap PDF dengan **Actual size / 100%** (BUKAN "Fit to page").
Satu PDF = satu template = satu jenis kertas, jadi letak satu jenis kertas
dalam tray setiap kali. `make` (tanpa arahan) memaparkan menu.

Fail CSV yang masih kosong hanya memberi AMARAN dan dilangkau.

## Format data (CSV)

UTF-8, baris pertama ialah tajuk lajur: `nama,nokp,template`

| Lajur | Maksud | Contoh |
|---|---|---|
| nama | nama penuh | AHMAD BIN ALI |
| nokp | No. KP atau nombor pengenalan lain, **apa-apa format** (amaran hanya jika kosong) | 900101-14-5555, A12345678 |
| template | nama seksyen dalam config.txt | kursus-a |

Satu fail CSV boleh mengandungi baris untuk beberapa template. Setiap nama
dalam lajur `template` mesti ada seksyen `[nama]` yang sama dalam `config.txt`.

Eksport dari LibreOffice Calc: *Save As > Text CSV*, set Character set = UTF-8.
Contoh: `data/contoh.csv`.

## Template baharu (penyelia)

1. Ukur ruang kosong dengan pembaris dari **sudut kiri atas** kertas:
   `x1 y1 x2 y2` (kiri, atas, kanan, bawah) dalam mm.
2. Salin satu seksyen dalam `config.txt`, tukar nama dan ukuran.
   Nama ini juga menjadi nama fail PDF (`output/<nama>.pdf`).
3. `make ujian`, kemudian cetak `output/ujian-<nama>.pdf` pada kertas biasa.
4. Tindih pada sijil kosong dan halakan ke cahaya. Kotak biru mesti
   sepadan dengan ruang kosong.

## Kalibrasi printer

Jika semua cetakan anjak ke arah yang sama, ukur jaraknya (contoh: garisan
"100" jatuh di 101.5 mm) dan tetapkan `offset_mm = -1.5 0` dalam `[global]`.

## Masalah lazim

| Mesej / gejala | Punca dan penyelesaian |
|---|---|
| `template '...' tiada dalam config` | ejaan lajur `template` dalam CSV tidak sama dengan `[nama]`. Mesej menyenaraikan nama yang ada |
| `kunci tidak dikenali` | taip silap kunci dalam config.txt (lihat senarai di kepala fail) |
| `kotak_mm keluar dari kertas` | `kertas_mm` salah (landskap/potret?) atau ukuran terbalik |
| `nokp kosong` (amaran) | sel No. KP tertinggal dalam CSV |
| `fail kosong atau tiada baris data` (amaran) | CSV tiada baris data. Fail itu dilangkau |
| `TERLEBIH` dalam ringkasan | nama terlalu panjang walau saiz min: besarkan kotak atau kecilkan `nama_saiz` min |
| Font salah | semak `fc-list : family | sort -u`; nama font salah akan diganti senyap oleh cairo |
| Cetakan tersasar | pilih Actual size 100% dan buat kalibrasi |

## Pembangun

`Main.hs` dibahagi kepada: JENIS DATA, FUNGSI TULEN (`pure_*`, tiada kesan
sampingan), FUNGSI IO (`io_*`), TITIK MASUK. Peta penuh di kepala `Main.hs`.
Kebergantungan: lihat `shell.nix`.

## Privasi

`data/*` dan `output/` mengandungi No. KP sebenar dan diabaikan oleh git.
Hanya `data/contoh.csv` (rekaan) dibenarkan masuk repositori.
