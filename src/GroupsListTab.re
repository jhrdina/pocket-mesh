let render = () =>
  MaterialUi.(
    <List>
      <GroupRow
        alias="Personal"
        membersPreview=[
          "Jan Hrdina - Laptop",
          "Jan Hrdina - Desktop",
          "Jan Hrdina - Phone",
        ]
        membersCount=3
      />
      <GroupRow
        alias="School Team X"
        membersPreview=["Peter", "Carl", "Tony", "Jose"]
        membersCount=4
      />
    </List>
  );

let renderFab = (~className) =>
  MaterialUi.(<Fab color=`Secondary className> <Icons.Add /> </Fab>);