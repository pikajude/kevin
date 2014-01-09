set :stage, :staging

server '192.168.0.3',
  user: 'damn',
  roles: %w{app},
  ssh_options: {
    user: 'damn',
    keys: "#{ENV["HOME"]}/.ssh/damn"
  }
