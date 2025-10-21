target "default" {
  context = "."
  dockerfile = "dockerfile"
  tags = ["zad8:latest"]
  no-cache = true
}