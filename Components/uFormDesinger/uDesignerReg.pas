unit uDesignerReg;

interface
  uses Classes, uDesigner, uPropertyGrid, uPropertyManager, uComponentList;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RttiDesign', [
    TuFormDesigner,
    TuPropertiesManager,
    TuPropertyGrid,
    TuComponentList
  ]);
end;

end.
