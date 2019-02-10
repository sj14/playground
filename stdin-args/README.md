# stdin-args

A Go example to parse input from the standard input or from an argument.

## stdin (directly)

```text
$ go run main.go
I'm running the tool and now it waits for my input which is ended with a new line.
> I'm running the tool and now it waits for my input which is ended with a new line.
```

## stdin (pipe)

```text
$ echo from the pipe | go run main.go
> from the pipe
```

## argument

```text
$ go run main.go "the input is the first argument"
> the input is the first argument
```