FROM nvidia/cuda:12.3.1-devel-ubuntu22.04

# Set DEBIAN_FRONTEND to noninteractive to prevent tzdata configuration dialog
ENV DEBIAN_FRONTEND=noninteractive

USER root

RUN mkdir /app && cd /app
# Set the working directory
WORKDIR /app
# Mark /app as a volume to be mounted
VOLUME /app

# Install gnupg
RUN apt-get update && apt-get upgrade -y && \
    apt-get install -y gnupg cmake && \
    apt-get install -y build-essential dkms wget apt-utils && \
    apt-get install -y pkg-config libcairo2-dev && \
    apt-get install -y software-properties-common && \
    apt-get install -y python3-cairo-dev && \
    apt-get install -y netstat-nat telnet && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Download and install pip for Python 3 using get-pip.py script
RUN apt-get update && apt-get install -y curl python3-venv
RUN apt-get install -y python3-dev
RUN ln -s /usr/bin/python3 /usr/bin/python
# Add a line to activate the virtual environment in ~/.bashrc
RUN echo "source /myvenv/bin/activate" >> /root/.bashrc

# Copy requirements and install Python packages
COPY requirements.txt .

# Create and activate a virtual environment
RUN python3 -m venv /myvenv && \
    . /myvenv/bin/activate && \
    pip install pycairo && \
    pip install -r requirements.txt

EXPOSE 8888

# Copy the start script and make it executable
COPY start.sh .
RUN chmod +x start.sh

# Start your application with CMD
CMD ["source /myvenv/bin/activate", "/bin/bash", "/app/start.sh"]

