unit SelectStorageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TSelectStorage = class(TForm)
    GroupBox1: TGroupBox;
    rbDBStorage: TRadioButton;
    rbUnboundStorage: TRadioButton;
    Button1: TButton;
    Button2: TButton;
  end;

implementation

{$R *.dfm}

end.
