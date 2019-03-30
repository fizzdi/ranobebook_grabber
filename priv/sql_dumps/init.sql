-- Роли
CREATE TABLE roles (
    id SERIAL PRIMARY KEY,
    description TEXT NOT NULL,
	mnemonic TEXT NOT NULL,
    deleted SMALLINT DEFAULT 0 NOT NULL
);

-- ===============================================
-- сущности
-- ===============================================
-- Пользователи
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    description TEXT NOT NULL,
    login TEXT NOT NULL,
    password TEXT NOT NULL,
    role_id INTEGER NOT NULL REFERENCES roles(id) ON DELETE RESTRICT ON UPDATE RESTRICT,
    deleted SMALLINT DEFAULT 0 NOT NULL,
    created_at TIMESTAMP WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP WITHOUT TIME ZONE DEFAULT CURRENT_TIMESTAMP NOT NULL
);

INSERT INTO roles(description, mnemonic) 
	VALUES ('Администратор', 'admin');
INSERT INTO users(description, login, password, role_id)
	VALUES ('Администратор', 'admin', 'admin', 1);