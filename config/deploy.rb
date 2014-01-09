set :application, 'damn'
set :repo_url, 'https://github.com/joelteon/kevin.git'

ROOT = '/srv/irc/damn'
set :deploy_to, ROOT

set :linked_dirs, []
set :log_level, :debug

namespace :deploy do
  desc 'Restart application'
  task :restart do
    on roles(:app), in: :sequence do
      execute :cabal, "update"
      within "#{ROOT}/current" do
        execute :cabal, "sandbox", "init"
        execute :cabal, "install", "--only-dependencies"
        execute :cabal, "build"
      end
      execute "sudo stop damn || true"
      sudo :start, "damn"
    end
  end
end
