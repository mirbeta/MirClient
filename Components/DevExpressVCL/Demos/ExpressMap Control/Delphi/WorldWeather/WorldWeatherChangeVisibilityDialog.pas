unit WorldWeatherChangeVisibilityDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, Menus,
  StdCtrls, cxButtons, cxContainer, cxEdit, cxCheckListBox;

type
  TWorldWeatherChangeVisibilityDialogForm = class(TForm)
    cxCheckListBox1: TcxCheckListBox;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WorldWeatherChangeVisibilityDialogForm: TWorldWeatherChangeVisibilityDialogForm;

implementation

{$R *.dfm}

end.
