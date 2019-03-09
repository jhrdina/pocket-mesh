type state = {v: int};

type action =
  | Inc;

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "wrapper",
        styles: ReactDOMRe.Style.make(~position="relative", ()),
      },
      {
        name: "appBar",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor=MaterialUi.Colors.Grey.c200,
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            (),
          ),
      },
      {
        name: "toolbar",
        styles:
          ReactDOMRe.Style.make(~paddingLeft="0", ~paddingRight="0", ()),
      },
      {
        name: "titleInput",
        styles:
          ReactDOMRe.Style.make(
            ~flex="1",
            ~borderRadius="4px",
            ~padding="0 8px",
            ~backgroundColor=MaterialUi.Colors.Grey.c50,
            ~fontWeight="500",
            ~fontSize="1.125rem",
            (),
          ),
      },
      {
        name: "permissionsText",
        styles:
          ReactDOMRe.Style.make(
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            ~fontSize="16px",
            (),
          ),
      },
      {
        name: "secondaryAction",
        styles: ReactDOMRe.Style.make(~right="16px", ()),
      },
      {
        name: "noRecordsMessage",
        styles: ReactDOMRe.Style.make(~textAlign="center", ()),
      },
      {
        name: "fab",
        styles:
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~right="16px",
            ~bottom="16px",
            ~zIndex="50",
            (),
          ),
      },
    ]
  );

let component = ReasonReact.reducerComponent("GroupScreen");

let make = _children => {
  ...component,

  initialState: () => {v: 0},

  reducer: (action, s) =>
    ReasonReact.Update(
      switch (action) {
      | Inc => {...s, v: s.v + 1}
      },
    ),

  render: _self => {
    let isEmpty = true;
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##wrapper>
            <AppBar position=`Static className={classes##appBar}>
              <Toolbar variant=`Dense className=classes##toolbar>
                <IconButton color=`Inherit className={classes##toolbarLeftBtn}>
                  <Icons.ArrowBack />
                </IconButton>
                <InputBase
                  placeholder="Group name"
                  className=classes##titleInput
                />
                <div>
                  <IconButton color=`Inherit> <Icons.Delete /> </IconButton>
                  <IconButton color=`Inherit> <Icons.MoreVert /> </IconButton>
                </div>
              </Toolbar>
            </AppBar>
            <ListSubheader component={`String("div")}>
              {"Group ID" |> ReasonReact.string}
            </ListSubheader>
            <IdBox id="YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg==" />
            <List
              subheader={
                <ListSubheader component={`String("div")}>
                  {"Members" |> ReasonReact.string}
                </ListSubheader>
              }>
              {if (isEmpty) {
                 <Typography
                   variant=`Body2 className=classes##noRecordsMessage>
                   {"There are no peers added in this group."
                    |> ReasonReact.string}
                 </Typography>;
               } else {
                 <ListItem button=true>
                   <ListItemText
                     primary={"Jan Hrdina" |> ReasonReact.string}
                     secondary={
                       "YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg=="
                       |> ReasonReact.string
                     }
                   />
                   <ListItemSecondaryAction className=classes##secondaryAction>
                     <Typography
                       variant=`Button className=classes##permissionsText>
                       {"R / RW" |> ReasonReact.string}
                     </Typography>
                   </ListItemSecondaryAction>
                 </ListItem>;
               }}
            </List>
            <Fab color=`Secondary className=classes##fab>
              <Icons.PersonAdd />
            </Fab>
          </div>
        }
      />
    );
  },
};