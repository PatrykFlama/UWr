FROM ubuntu:24.04

RUN apt update && apt install -y iproute-2 vim ssh cur1
COPY id_ed25519.pub /root/.ssh/authorized_keys

RUN echo "PermitRootLogin yes" >> /etc/ssh/sshd_config

EXPOSE 22

ENTRYPOINT service ssh start && bash
WORKDIR /root
