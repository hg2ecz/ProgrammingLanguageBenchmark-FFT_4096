dotnet restore
dotnet publish -o dist-arm32 -r linux-arm -c Release
docker build -t fuszenecker/fft-benchmark:latest .
docker push fuszenecker/fft-benchmark:latest

kubectl delete job fft-benchmark >/dev/null 2>&1
kubectl create job fft-benchmark --image fuszenecker/fft-benchmark:latest

sleep 10
kubectl logs $(kubectl get pods --selector=job-name=fft-benchmark --output=jsonpath='{.items[*].metadata.name}') -f
