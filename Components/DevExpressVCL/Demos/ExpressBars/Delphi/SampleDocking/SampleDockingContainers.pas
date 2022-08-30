unit SampleDockingContainers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, dxDockPanel, dxDockControl, cxPropertiesStore;

type
  TSampleDockingContainersForm = class(TForm)
    cxPropertiesStore2: TcxPropertiesStore;
    dxDockPanel2: TdxDockPanel;
    dxDockSite1: TdxDockSite;
    dxLayoutDockSite1: TdxLayoutDockSite;
    dxDockPanel3: TdxDockPanel;
    dxDockPanel4: TdxDockPanel;
    dxTabContainerDockSite2: TdxTabContainerDockSite;
    dxVertContainerDockSite1: TdxVertContainerDockSite;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SampleDockingContainersForm: TSampleDockingContainersForm;

implementation

{$R *.dfm}

end.
