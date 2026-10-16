# ============================================================
# Makefile - menu arahan untuk hsijil. Taip `make` untuk lihat menu.
#
# Fail data dinyatakan SECARA EKPLISIT di bawah (tiada imbasan folder).
# Untuk guna data lain tanpa sunting fail ini:
#     make jana DATA=data/contoh.csv
# ============================================================

# Guna '>' sebagai awalan arahan (elak masalah Tab)
.RECIPEPREFIX = >

CONFIG := config.txt
OUT    := output
DATA   := data/penghargaan.csv data/penyertaan.csv

.PHONY: bantuan bina semak ujian jana bersih
.DEFAULT_GOAL := bantuan

bantuan:
> @echo "hsijil - cetak nama + No. KP pada kertas sijil"
> @echo ""
> @echo "  make bina    susun program (sekali sahaja, atau selepas ubah Main.hs)"
> @echo "  make semak   semak config.txt + CSV (tidak jana PDF, selamat)"
> @echo "  make ujian   jana PDF kalibrasi ke $(OUT)/ujian-*.pdf"
> @echo "  make jana    jana sijil sebenar ke $(OUT)/<template>.pdf"
> @echo "  make bersih  padam binari, build/ dan $(OUT)/"
> @echo ""
> @echo "  Data guna : $(DATA)"
> @echo "  Tukar data: make jana DATA=data/contoh.csv"
> @echo "  Panduan penuh: README.md  atau  ./jana-sijil bantuan"

jana-sijil: Main.hs
> ghc -O2 -outputdir build Main.hs -o jana-sijil

bina: jana-sijil

semak: jana-sijil
> ./jana-sijil semak $(CONFIG) $(DATA)

ujian: jana-sijil
> ./jana-sijil ujian $(CONFIG) $(OUT)

jana: jana-sijil
> ./jana-sijil jana $(CONFIG) $(OUT) $(DATA)

bersih:
> rm -rf build jana-sijil $(OUT)
