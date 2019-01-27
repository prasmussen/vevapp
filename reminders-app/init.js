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
    };

    app.ports.toJavascript.subscribe(function(msg) {
        update(msg, model);
    });

    // Listen for sign in events from elm
    //app.ports.signIn.subscribe(function() {
    //    auth.signIn();
    //});

    //// Listen for sign out events from elm
    //app.ports.signOut.subscribe(function() {
    //    auth.signOut();
    //});

    // Listen for sign-in state changes and send to elm
    auth.isSignedIn.listen(function() {
        console.log("auth changed", maybeUserFromAuth(auth));
        //sendUser(auth);
    });


}

function update(msg, model) {
    switch (msg.tag) {
        case 'ListReminders':
            var options = msg.data;

            gapi.client.calendar.events.list(options).then(function(res) {
                console.log("success", res.result);
                model.app.ports.fromJavascript.send(toMsg("ListRemindersSuccess", res.result));
            }, function(res) {
                console.log("failure", res);
                // TODO: send res.result instead
                model.app.ports.fromJavascript.send(toMsg("ListRemindersFailure", res.result.error.message));
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

function listReminders(successTag, errorTag, options) {
    var now = new Date();


    listReminders().then(function(res) {
        var items = res.result.items.map(formatReminder);
        app.ports.listRemindersSuccess.send(items);
    }, function(res) {
        app.ports.listRemindersFailed.send(res.result.error.message);
    });
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
