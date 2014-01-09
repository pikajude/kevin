set :stage, :production

server 'joelt.io',
  user: 'deploy',
  roles: %w{app},
  ssh_options: {
    user: 'deploy',
    keys: "#{ENV["HOME"]}/.ssh/id_rsa",
    port: 5423
  }
