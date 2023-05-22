function enthx --wraps='GOFLAGS="-tags=ent,consul-ent,consulent" hx' --description 'alias enthx=GOFLAGS="-tags=ent,consul-ent,consulent" hx'
  GOFLAGS="-tags=ent,consul-ent,consulent" hx $argv
        
end
