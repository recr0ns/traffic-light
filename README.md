Тестовое задание: **Traffic Light**

Запуск сервиса в docker контейнере:
- ```git clone https://github.com/recr0ns/traffic-light.git```
- ```cd ./traffic-light``` 
- ```chmod +x ./rebar```
- ```make release```
- ```docker build -t traffic-light .```
- ```docker run -ti -p 8090:8090 traffic-light /bin/bash```
- ```./build.sh start```

*Остановка*: ```./build.sh stop```

*Ping*: ```./build.sh ping```
