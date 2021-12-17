"use strict"

exports._setAppContext = function (app, context) {
    return function () {
        app.locals.context = context
        context = context;
    }
}

exports._getHandlerMAppContext = function (req) {
    return function () {
        return req.app.locals.context;
    }
}

exports._getAppMAppContext = function (app) {
    return function () {
        return app.locals.context
    }
}

exports._getRouterMAppContext = function (app, router) {
    return function () {
        return app.locals.context
    }
}

exports._mkRouter = function() {
    return require('express').Router();
}

exports._useRouter = function (app, path, router) {
    return function () {
        app.use(path, router);
    }
}

exports._http = function (router, method, route, handler) {
    return function () {
        router[method](route, function(req, resp, next) {
            return handler(req)(resp)(next)();
        });
    };
};

exports._use = function (router, mw) {
    return function () {
        router.use(function(req, resp, next) {
            return mw(req)(resp)(next)();
        });
    };
};
