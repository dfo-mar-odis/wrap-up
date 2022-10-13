from tkinter import *


class TkGui:
    def __init__(self):
        # create a tkinter app
        self.master = Tk()
        self.master.geometry("400x400")
        self.master.title("Wrap Up Demo")

        # add a user input field:
        self.text_box = Text(self.master, height=2, width=10)
        self.text_box.pack()

        self.screen_string = StringVar()
        self.screen_string.set("")
        self.string_label = Label(self.master, textvariable=self.screen_string)

        print_btn = Button(self.master, text="Print input text", command=self.print_text)
        print_btn.pack()

        close_btn = Button(self.master, text="Exit", command=self.master.destroy)
        close_btn.pack()
        self.string_label.pack()
        self.master.mainloop()

    def print_text(self):
        self.screen_string.set(self.text_box.get('1.0', 'end'))
