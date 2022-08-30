unit SimpleVerticalGridDemoData;

interface

uses
  SysUtils, Classes, DB, cxStyles, cxClasses, cxVGrid, Forms, dxmdaset;

type
  TSimpleVerticalGridDemoMainDM = class(TDataModule)
    cxStyleRepository: TcxStyleRepository;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
  end;

var
  SimpleVerticalGridDemoMainDM: TSimpleVerticalGridDemoMainDM;

implementation

{$R *.dfm}

end.
