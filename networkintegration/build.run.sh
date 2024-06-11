set -e 
context=lambda
absolute_path_to_output_folder=/data/ibartha/networkprop/paper
#build
id=$(docker --context $context build -q -f Dockerfile . )

echo $id
#execute
docker --context $context run  --gpus 1 -v $absolute_path_to_output_folder:/out $id
