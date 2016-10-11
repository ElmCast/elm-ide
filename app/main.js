const cp = require('child_process')
const elm = require('./elm.js')
const neovim = require('neovim-client')
const electron = require('electron')

const app = elm.Main.fullscreen()

function notify(method, name, data) {
    const notification = { [method]: { [name]: data } }
    app.ports.notifications.send(JSON.stringify(notification))
}

const nvim = cp.spawn('nvim', ['--embed'])

nvim.on('error', (err) => {
    notify('system', 'error', 'Failed to spawn neovim process: ' + err.message)
})

neovim(nvim.stdin, nvim.stdout, (err, api) => {
    if (err !== null) {
        notify('system', 'error', 'Failed to establish RPC connection to neovim: ' + err.message)
    }

    api.on('request', (method, args, resp) => {
    })

    api.on('notification', (method, args) => {
        args.forEach(data => {
            var name = data.shift()
            notify(method, name, data)
        })
    })

    api.on('disconnect', () => {
        notifiy('system', 'disconnect', 'RPC connection to neovim was lost')
    })


    app.ports.command.subscribe(message => {
        const { command, data } = JSON.parse(message)
        switch (command) {
            case 'attach':
                api.uiAttach(data.columns, data.lines, true, (err) => {
                    if (err != null) {
                        notify('system', 'error', 'Failed to attach to neovim UI: ' + err.message)
                    }
                })
                api.command('doautocmd <nomodeline> GUIEnter')
                break

            case 'set-title':
                document.title = data
                break

            case 'bell':
                electron.shell.beep()
                break
        }
    })

    notify('system', 'ready', 'RPC connection to neovim is ready')
})
