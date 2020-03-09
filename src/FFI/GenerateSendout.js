"use strict";

const x = {
    message: {
        id: "dlg132",
        text: "hi"
    },
    recipients: [{
        user: {
            id: "dmitry"
        },
        status: "NONE"
    }]
};

exports._generateSendout = () => JSON.stringify(x);
