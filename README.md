# hpsch

A todo list for recurring tasks that chooses something
for you to do next.

## Building and Running

You need to have [Stack](https://docs.haskellstack.org/en/stable/README/) installed.
In order to build, use `stack build`.
To run, `stack run`.

## Usage

The program reads tasks from a the JSON file `taskfile`.
The `taskfile` should contain an array with tasks,
like the example.

```
[
    {
        "interval": 7,
        "date": "2020-11-04",
        "name": "Nielsen's DL Book",
        "info": "Continue reading second half of chapter 1"
    },
    {
        "interval": 1,
        "date": "2020-11-04",
        "name": "Daily programming practice",
        "info": "Do programming problems for 30 minutes"
    },
    {
        "interval": 3,
        "date": "2020-11-03",
        "name": "Automata Theory Book",
        "info": "Continue reading second half of chapter 3"
    },
    {
        "name": "OS Project",
        "interval": 7,
        "date": "2020-11-01",
        "deadline": "2020-11-25",
        "info": "Check out the description for OS class project #2"
    }
]
```

A task has five fields:

- Name: The name of the task or project
- Interval: Defines in how many days the task will pop up again
- Date: The next date in which the task will show up
  If the task is overdue, it will show up the next
  time you run the program.
- Deadline (Optional): Dictates until when a task will repeat.
- Info: Field for description or reminders for your future self.

Upon running the program, you will be presented by a task.
You then have a few options:

- `d`: Finish task and write new description
- `D`: finish task and maintain current description
- `x`: set task aside
- `l`: push task to end of queue
- `i`: modify the interval for the current task
- `q`: quit (pending tasks are delegated)
- `?`: show commands

When a task is finished, unless the deadline is surpassed,
will show up in a number of days from now dictated by it's
interval.
When you set a task aside, it will show up the next time
you run the program.
When you push a task to the end of the queue, it will show
up later in the session.
