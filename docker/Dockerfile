FROM ubuntu:16.04

# Install APT dependencies.
ENV DEBIAN_FRONTEND "noninteractive"
RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential libglfw-dev \
	make sbcl \
 && rm -rf /var/lib/apt/lists/*
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl \
 && rm -rf /var/lib/apt/lists/*

# Install QuickLisp.
RUN curl -L https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp \
 && yes "" | sbcl \
	--load /tmp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install)" \
	--eval "(ql:add-to-init-file)" \
	--eval "(quit)" \
 && rm /tmp/quicklisp.lisp
