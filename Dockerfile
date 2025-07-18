FROM ubuntu:jammy

ARG DEBIAN_FRONTEND=noninteractive
RUN apt update && \
	apt install -y software-properties-common && \
	apt-add-repository -y ppa:ansible/ansible && \
	apt update && \
	apt install -y ansible build-essential

COPY . .

CMD ["sh", "-c", "ansible-playbook $TAGS local.yml"]
