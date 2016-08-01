FROM nginx

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

WORKDIR /usr/share/nginx/html

COPY dist/index.html index.html
COPY src/css css
COPY src/fonts fonts
COPY src/js js
COPY src/images images

COPY dist/elm.js dist/elm.js
