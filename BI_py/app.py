from flask import Flask, render_template, request, redirect, url_for, flash, get_flashed_messages
from flask_mail import Mail, Message
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import Column, Integer, String
from werkzeug.security import generate_password_hash, check_password_hash
import pyodbc as bd
import random
import string

app = Flask(__name__)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.secret_key = 'secreto'  # Atualize com uma chave secreta segura na produção
app.config['MAIL_SERVER'] = 'smtp-mail.outlook.com'  # Configurar com o servidor SMTP apropriado
app.config['MAIL_PORT'] = 587
app.config['MAIL_USE_TLS'] = True
app.config['MAIL_USE_SSL'] = False
app.config['MAIL_USERNAME'] = 'silvania@novadistribuidorane.com.br'  # Substituir com seu e-mail
app.config['MAIL_PASSWORD'] = 'CasaVerde#1020'  # Substituir com sua senha
app.config['MAIL_DEFAULT_SENDER'] = 'silvania@novadistribuidorane.com.br'

mail = Mail(app)

app.config['SQLALCHEMY_DATABASE_URI'] = 'mssql+pyodbc://sa:senha@1234@NOVACE-TI\DEV/TESTE?driver=ODBC+Driver+17+for+SQL+Server'

db = SQLAlchemy(app)

class User(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(80), unique=True, nullable=False)
    email = db.Column(db.String(120), unique=True, nullable=False)
    setor = db.Column(db.String(120))
    password = db.Column(db.String(255), nullable=False)

# Criar as tabelas
with app.app_context():
    db.create_all()

def generate_random_password():
    # Gera uma senha aleatória de 5 caracteres
    characters = string.ascii_letters + string.digits + string.punctuation
    return ''.join(random.choice(characters) for i in range(5))

def send_email(user_email, username, password):
    # Obter informações do e-mail antes do "@"
    email_prefix = user_email.split('@')[0]

    # Enviar e-mail
    msg = Message('Dados de Acesso', recipients=[user_email])
    msg.body = f'Olá {username},\n\nSeus dados de acesso são:\n\nUsuário: {email_prefix}\nSenha: {password}\n\nAtenciosamente,\nSua Aplicação'
    mail.send(msg)

class MyView(db.Model):
    __tablename__ = 'SANDOZ_OL2'  # Nome da tabela no banco de dados
    __table_args__ = {'schema': 'TESTE'}  # Nome do esquema no banco de dados, opcional se for 'dbo'
    # Defina os campos da view como colunas do modelo
    CODIGO     = db.Column(db.Integer)
    COD_EAN    = db.Column(db.String(13))
    DESCRICAO  = db.Column(db.String(80))
    Lis        = db.Column(db.String(5))
    Cod_ClaTri = db.Column(db.String(4))
    Fabricante = db.Column(db.String(25))
    DANFE      = db.Column(db.String(80))
    Dat_Ent    = db.Column(db.String(80))
    ICMS       = db.Column(db.String(80))
    C_FIXO     = db.Column(db.String(80))
    C_VENDA    = db.Column(db.String(80))
    I_FEDERAL  = db.Column(db.String(80))
    INVEST     = db.Column(db.String(80))
    P_C_RESC   = db.Column(db.String(80))
    Markup     = db.Column(db.String(80))
    TOTAL      = db.Column(db.String(80))
    P_VENDA    = db.Column(db.String(80))
    DESC       = db.Column(db.String(80))
    L_LIQ      = db.Column(db.String(42))
    Est_Dispo  = db.Column(db.String(4))

@app.route('/')
def index():
    return 'Página inicial'

@app.route('/login', methods=['GET', 'POST'])
def login():
    if request.method == 'POST':
        username = request.form['username']
        password = request.form['password']
        user = User.query.filter_by(username=username).first()

        if user and check_password_hash(user.password, password):
            # Login bem-sucedido
            flash('Login bem-sucedido!', 'success')
            return redirect(url_for('index'))
        else:
            # Login falhou
            flash('Credenciais inválidas. Tente novamente.', 'danger')

    return render_template('login.html', messages=get_flashed_messages())

@app.route('/cadastro', methods=['GET', 'POST'])
def cadastro():
    if request.method == 'POST':
        username = request.form['username']
        email = request.form['email']
        setor = request.form['setor']

        # Verificar se o usuário ou email já existe
        if User.query.filter_by(username=username).first() or User.query.filter_by(email=email).first():
            flash('Usuário ou email já existe. Escolha outro nome de usuário ou email.', 'danger')
        else:
            # Gerar senha aleatória
            password = generate_random_password()

            # Criar um novo usuário
            new_user = User(username=username, email=email, setor=setor, password=generate_password_hash(password, method='sha256'))
            db.session.add(new_user)
            db.session.commit()

            # Enviar email com login e senha
            send_email(email, username, password)

            flash('Cadastro realizado com sucesso! Verifique seu email para obter as credenciais de login.', 'success')
            return redirect(url_for('login'))

    return render_template('cadastro.html', messages=get_flashed_messages())

@app.route('/dashboard')
def dashboard():
    dados_da_view = MyView.query.all()  # Ou use query.filter para condições específicas
    return render_template('dashboard.html', dados=dados_da_view)

if __name__ == '__main__':
    app.run(debug=True)
