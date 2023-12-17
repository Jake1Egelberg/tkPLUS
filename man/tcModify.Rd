\name{tcModify}
\alias{tcModify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Modify an existing widget
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Use tcModify() to modify the displayed values of an existing widget.
}
\usage{
tcModify(widget_name, widget_list, ..., pos)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{widget_name}{
%%     ~~Describe \code{x} here~~
A string specifying the widget that you would like to modify.
}
\item{widget_list}{
The widget list containing the widget to modify.
}
\item{...}{,
A new argument. For example: \code{text="new text"}.
}
\item{pos}{
Do not change.
}
}
\examples{


# Set theme
tcTheme("DEFAULT")

.GlobalEnv$bind_fun<-function(){
  print("hi")
}

.GlobalEnv$get_data_test<-function(){

  print(tcGet("combo1",
              widget_list))
  print(tcGet("listbox1",
              widget_list))
  print(tcGet("entry1",
              widget_list))

}

# Create function to modify label
.GlobalEnv$modify_label<-function(){

  #Modify label
  tcModify(widget_name="label1",
           widget_list=widget_list,
           text="new text!",
           fg="red",
           background="blue",
           size=15,
           weight="bold",
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"))

  #Modify border
  tcModify(widget_name="frame1",
           widget_list=widget_list,
           border=tcBorder(2,"green"))

  #Modify combo
  tcModify("combo1",
           widget_list,
           weight="bold",
           values=c("my","name","is","jake"),
           selected="my",
           width=7,
           justify="left",
           foreground="blue",
           size=12,
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"))

  #modify entry
  tcModify("entry1",
           widget_list,
           text="ENTRY",
           width=5,
           justify="left",
           fg="red",
           background="black",
           size=10,
           weight="bold",
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"))

  #Modify check
  tcModify("check",
           widget_list,
           text="new CHECK",
           value=TRUE,
           fg="red",
           size=14,
           weight="bold",
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"),
           background="black")

  #Modify button
  tcModify("button2",
           widget_list,
           fg="red",
           background="black",
           size=10,
           weight="bold",
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"))

  .GlobalEnv$newplot<-ggplot(data.frame(x=1:10,y=(1:10)^3),aes(x=x,y=y))+
    geom_point()

  #Modify plot
  tcModify("plot1",
           widget_list,
           ggplot="newplot",
           hscale=0.5,
           vscale=0.5,
           binding=tcBind("<Enter>","bind_fun"))

  #Modify listbox
  tcModify("listbox1",
           widget_list,
           weight="bold",
           height=3,
           width=5,
           selectmode="multiple",
           fg="green",
           background="blue",
           size=10,
           family="Comic Sans MS",
           binding=tcBind("<Enter>","bind_fun"),
           add_scrollbar=FALSE,
           values=c("a","new","value"))


}

# Create widgets
frame<-tcFrame("app",
               tcGrid(1,1,padx=20,pady=10),
               "frame1",border=tcBorder(2,"red"))
label<-tcLabel("frame1",
               tcGrid(1,1),
               "label1",
               text="Im a label!")
button<-tcButton("frame1",
                 tcGrid(1,2,sticky="w"),
                 "button1",
                 text="Click me!",
                 command="modify_label")
listbox<-tcListbox("frame1",
                   tcGrid(2,1,yspan=2,padx=10),
                   name="listbox1",
                   values=c("Value 1",
                            "Value 2"))
combobox<-tcCombobox("frame1",
                     tcGrid(3,1,yspan=2),
                     name="combo1",
                     values=c("test 1", "test 2"),
                     selected="test 1")
entry<-tcEntry("frame1",
               tcGrid(3,3,pady=5),
              name="entry1")
check<-tcCheckbox("frame1",
                  tcGrid(3,4),
                  name="check",
                  text="check")
button2<-tcButton("frame1",
                  tcGrid(2,4),
                  "button2",
                  text="hi",
                  command="get_data_test")
.GlobalEnv$testplot<-ggplot(data.frame(x=1:10,y=1:10),aes(x=x,y=y))+
  geom_point()

tkplot<-tcPlot("frame1",
               tcGrid(5,1,yspan=10),
               "plot1",
               ggplot="testplot")

widget_list<-list()
widget_list[[1]]<-frame
widget_list[[2]]<-label
widget_list[[3]]<-button
widget_list[[4]]<-listbox
widget_list[[5]]<-combobox
widget_list[[6]]<-entry
widget_list[[7]]<-check
widget_list[[8]]<-button2
widget_list[[9]]<-tkplot

tcBuild("app",widget_list,"600x300",self_destruct = FALSE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
