function entvim --wraps='GOFLAGS="-tags=ent,consul-ent,consulent" nvim' --description 'alias entvim=GOFLAGS="-tags=ent,consul-ent,consulent" nvim'
  GOFLAGS="-tags=ent,consul-ent,consulent" nvim $argv
        
end
