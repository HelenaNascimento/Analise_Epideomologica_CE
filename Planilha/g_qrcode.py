import qrcode

# Link que você quer transformar em QR code
link = "https://github.com/HelenaNascimento"

# Cria o objeto QRCode
qr = qrcode.QRCode(
    version=1,
    error_correction=qrcode.constants.ERROR_CORRECT_L,
    box_size=10,
    border=4,
)

# Adiciona o link ao QR code
qr.add_data(link)
qr.make(fit=True)

# Cria uma imagem do QR code
img = qr.make_image(fill_color="green", back_color="white")

# Salva a imagem
#img.save("qrcode_linkedin.png")

# Exibe a imagem
img.show()
