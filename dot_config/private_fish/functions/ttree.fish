# Defined via `source`
function ttree --wraps=tree\ -I\ \'node_modules\|target\' --description alias\ ttree=tree\ -I\ \'node_modules\|target\'
  tree -I 'node_modules|target' $argv; 
end
