FROM node:8.1.2-alpine
ADD . /usr/src/app
RUN    cd /usr/src/app \
    && npm install
EXPOSE 8888
CMD ["node", "/usr/src/app"]
