FROM ubuntu:16.04

# Install APT dependencies.
ENV DEBIAN_FRONTEND "noninteractive"
RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential ca-certificates cmake curl \
	git libglfw3-dev make python3 rsync sbcl \
 && rm -rf /var/lib/apt/lists/*
RUN ln -s /usr/bin/python3 /usr/local/bin/python

# Install shaderc.
RUN curl -L https://storage.googleapis.com/shaderc/artifacts/prod/graphics_shader_compiler/shaderc/linux/continuous_clang_release/174/20191009-124722/install.tgz | tar zx \
 && rsync -Pa install/ /usr/local/ \
 && rm -rf install
ENV SHADERC_LIB_DIR=/usr/local/lib

# Install QuickLisp.
RUN curl -L https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp \
 && yes "" | sbcl \
	--load /tmp/quicklisp.lisp \
	--eval "(quicklisp-quickstart:install)" \
	--eval "(ql:add-to-init-file)" \
	--eval "(quit)" \
 && rm /tmp/quicklisp.lisp

# Install Rust.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs -o /tmp/rustup.sh \
 && sh /tmp/rustup.sh -y \
 && rm /tmp/rustup.sh

# Add build stuff.
ADD build.sh /
VOLUME /code
WORKDIR /code
