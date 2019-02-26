type state = {v: int};

type action =
  | Inc;

let idBoxBorder = "1px dashed #ccc";

let muiStyles: MaterialUi.Theme.t => list(MaterialUi.WithStyles.style) =
  theme => [
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
      styles: ReactDOMRe.Style.make(~paddingLeft="0", ~paddingRight="0", ()),
    },
    {
      name: "titleInput",
      styles:
        ReactDOMRe.Style.make(
          ~flex="1",
          ~borderRadius="4px",
          ~padding="0 8px",
          ~backgroundColor=MaterialUi.Colors.Grey.c50,
          (),
        ),
    },
    {
      name: "idBox",
      styles:
        ReactDOMRe.Style.make(
          ~margin="0 16px",
          ~display="flex",
          ~border=idBoxBorder,
          ~borderRadius="4px",
          (),
        ),
    },
    {
      name: "idBoxId",
      styles:
        ReactDOMRe.Style.make(
          ~flex="1",
          ~fontSize="14px",
          ~fontFamily="monospace",
          ~color=
            MaterialUi.Theme.(
              theme
              |> Theme.paletteGet
              |> Palette.textGet
              |> TypeText.secondaryGet
            ),
          ~padding="8px",
          ~borderRight=idBoxBorder,
          ~wordBreak="break-all",
          ~lineHeight="20px",
          (),
        ),
    },
    {
      name: "idBoxCopyBtn",
      styles: ReactDOMRe.Style.make(~borderRadius="0 4px 4px 0", ()),
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
  ];

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
      <WithStyles
        classesWithTheme=muiStyles
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
                <div className=classes##toolbarRightBlock>
                  <IconButton color=`Inherit> <Icons.Delete /> </IconButton>
                  <IconButton color=`Inherit> <Icons.MoreVert /> </IconButton>
                </div>
              </Toolbar>
            </AppBar>
            <ListSubheader component={`String("div")}>
              {"Group ID" |> ReasonReact.string}
            </ListSubheader>
            <div className=classes##idBox>
              <div className=classes##idBoxId>
                {"YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg==" |> ReasonReact.string}
              </div>
              <Button className=classes##idBoxCopyBtn>
                {"Copy" |> ReasonReact.string}
              </Button>
            </div>
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