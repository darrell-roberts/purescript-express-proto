# Purescript express prototype

A simple prototype exploring the possibility of building a micro-service framework off of a purescript-express package.

# Example

## Run
```
spago run -b "-p 8081 --appName psms --loglevel debug"
```

## Test route
```
curl -s -H "x-requestid: myid"  http://localhost:8081/user/users |jq
```

## Test failure
```
curl -s http://localhost:8081/fail |jq
```