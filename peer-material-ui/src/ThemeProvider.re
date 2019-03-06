let component = ReasonReact.statelessComponent("ThemeProvider");
let make = children => {
  ...component,
  render: _self =>
    <MaterialUi.ThemeProvider
      theme={MaterialUi_Theme.create(
        MaterialUi.ThemeOptions.(
          make(
            ~typography=Typography.make(~useNextVariants=true, ()),
            ~palette=
              PaletteOptions.make(
                ~primary=
                  Primary.make(
                    ~main="#616161",
                    ~light="#8e8e8e",
                    ~dark="#373737",
                    ~contrastText="#ffffff",
                    (),
                  ),
                ~secondary=
                  Secondary.make(
                    ~main="#7bb241",
                    ~light="#ade470",
                    ~dark="#4a820c",
                    ~contrastText="#ffffff",
                    (),
                  ),
                (),
              ),
            (),
          )
        ),
      )}>
      ...children
    </MaterialUi.ThemeProvider>,
};