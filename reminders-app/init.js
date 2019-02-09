// TODO: handle err
function init(auth, err) {
    var maybeUser = maybeUserFromAuth(auth);

    console.log("user", maybeUser);
    console.log("err", err);


    var app = Elm.Main.init({
        node: document.getElementById("app"),
        flags: maybeUser,
    });

    var model = {
        app: app,
        auth: auth,
    };

    // Listen for messages from elm
    app.ports.toJavascript.subscribe(function(msg) {
        update(msg, model);
    });

    // Listen for sign-in state changes
    auth.isSignedIn.listen(function() {
        update(toMsg("AuthChange"), model);
    });
}

function update(msg, model) {
    switch (msg.tag) {
        case 'AuthChange':
            var maybeUser = maybeUserFromAuth(model.auth);
            model.app.ports.fromJavascript.send(toMsg("AuthChange", maybeUser));
            break;

        case 'SignIn':
            model.auth.signIn();
            break;

        case 'SignOut':
            model.auth.signOut();
            break;

        case 'ListReminders':
            var options = msg.data;

            gapi.client.calendar.events.list(options).then(function(res) {
                model.app.ports.fromJavascript.send(toMsg("ListRemindersSuccess", res.result));
            }, function(res) {
                model.app.ports.fromJavascript.send(toMsg("ListRemindersFailure", res.result.error.message));
            });
            break;

        case 'CreateReminder':
            var options = msg.data;

            gapi.client.calendar.events.insert(options).then(function(res) {
                model.app.ports.fromJavascript.send(toMsg("CreateReminderSuccess", res.result));
            }, function(res) {
                model.app.ports.fromJavascript.send(toMsg("CreateReminderFailure", res.result.error.message));
            });
            break;

        default:
            console.warn("Unhandled message from elm: " + msg.tag);
    }
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
