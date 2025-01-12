#! /bin/bash

# https://transang.me/create-a-multiple-domains-self-signed-ssl-certificate-with-testing-scripts/
# https://github.com/civetweb/civetweb/blob/master/docs/OpenSSL.md

openssl genrsa -out server.key 4096
openssl req \
	-new \
	-key server.key \
	-nodes \
	-out server.csr \
	-subj "/C=ES/ST=Barcelona/L=Barcelona/O=webfakes.r-lib.org/CN=localhost" \
	-addext "subjectAltName=IP:127.0.0.1,DNS:localhost,DNS:localhost.localdomain"

openssl genrsa -out ca.key 4096
openssl req \
	-new \
	-x509 \
	-nodes \
	-days 36500 \
	-key ca.key \
	-out ca.crt \
	-subj "/C=ES/ST=Barcelona/L=Barcelona/O=webfakes.r-lib.org/OU=CA/CN=ca.webfakes.r-lib.org"

openssl x509 \
	-req \
	-in server.csr \
	-CAkey ca.key \
	-CA ca.crt \
	-set_serial -01 \
	-out server.crt \
	-days 36500 \
	-sha256 \
	-extfile <(printf "subjectAltName=IP:127.0.0.1,DNS:localhost,DNS:localhost.localdomain")

openssl verify -CAfile ca.crt server.crt

cp server.crt server.pem
cat server.key >> server.pem
