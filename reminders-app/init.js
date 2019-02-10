(function() {

    var strings = {
        AuthChange: "AuthChange",
        SignIn: "SignIn",
        SignOut: "SignOut",
        ListReminders: "ListReminders",
        ListRemindersSuccess: "ListRemindersSuccess",
        ListRemindersFailure: "ListRemindersFailure",
        CreateReminder: "CreateReminder",
        CreateReminderSuccess: "CreateReminderSuccess",
        CreateReminderFailure: "CreateReminderFailure",
    };

    function init(auth, err) {
        var maybeUser = maybeUserFromAuth(auth);
        var maybeError = formatError(err);

        var app = Elm.Main.init({
            node: document.getElementById("app"),
            flags: {
                user: maybeUser,
                error: maybeError,
            }
        });

        var model = {
            app: app,
            auth: auth,
        };

        // Listen for messages from elm
        app.ports.toJavascript.subscribe(function(msg) {
            update(msg, model);
        });

        if (auth) {
            // Listen for sign-in state changes
            auth.isSignedIn.listen(function() {
                var msg = toMsg(strings.AuthChange, model);
                update(msg, model);
            });
        }
    }

    function update(msg, model) {
        switch (msg.tag) {
            case strings.AuthChange:
                var maybeUser = maybeUserFromAuth(model.auth);
                sendMsg(model, strings.AuthChange, maybeUser);
                break;

            case strings.SignIn:
                model.auth.signIn();
                break;

            case strings.SignOut:
                model.auth.signOut();
                break;

            case strings.ListReminders:
                var options = msg.data;

                gapi.client.calendar.events.list(options).then(function(res) {
                    sendMsg(model, strings.ListRemindersSuccess, res.result);
                }, function(res) {
                    sendMsg(model, strings.ListRemindersFailure, res.result.error.message);
                });
                break;

            case strings.CreateReminder:
                var options = msg.data;

                gapi.client.calendar.events.insert(options).then(function(res) {
                    sendMsg(model, strings.CreateReminderSuccess, res.result);
                }, function(res) {
                    sendMsg(model, strings.CreateReminderFailure, res.result.error.message);
                });
                break;

            default:
                console.warn("Unhandled message from elm: " + msg.tag);
        }
    }


    function sendMsg(model, tag, data) {
        var msg = toMsg(tag, data);
        model.app.ports.fromJavascript.send(msg);
    }

    function toMsg(tag, data) {
        return {
            tag: tag,
            data: data,
        };
    }

    function maybeUserFromAuth(auth) {
        if (!auth) {
            return null;
        }

        var isAuthorized = auth.isSignedIn.get();
        if (!isAuthorized) {
            return null;
        }

        var user = auth.currentUser.get();
        var profile = user.getBasicProfile();

        return {
            email: profile.getEmail(),
        };
    }

    function formatError(error) {
        if (!error) {
            return null;
        }

        if (error.error.errors.length === 0) {
            console.error(error);
            return "Failed initializing google api";
        }

        var errors = error.error.errors.map(function(err) {
            return [
                "Domain: ",
                err.domain,
                ", reason: ",
                err.reason,
                ", message: ",
                err.message
            ].join("");
        });

        return errors[0];
    }

    gapi.load('client:auth2', function() {
        gapi.client.init({
            'apiKey': 'AIzaSyAYk1JO0wu3UxAbYmeDxLclOA2psEwLQZE',
            'discoveryDocs': ["https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest"],
            'clientId': '853474051422-rbvibfhir17apk19nq9vf3j8sshdo81f.apps.googleusercontent.com',
            'scope': 'https://www.googleapis.com/auth/calendar',
        }).then(function() {
            var auth = gapi.auth2.getAuthInstance();
            init(auth, null);
        }, function(err) {
            init(null, err)
        });
    });

})();
