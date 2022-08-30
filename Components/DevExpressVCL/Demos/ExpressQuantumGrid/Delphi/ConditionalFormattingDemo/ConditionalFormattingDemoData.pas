unit ConditionalFormattingDemoData;

interface

uses
  {$IFDEF CLR}
  System.ComponentModel,
  {$ENDIF}
  SysUtils, Classes, DB, cxStyles, cxClasses, cxGridTableView,
  Forms, DBClient, MidasLib, ImgList, Controls, cxGraphics;

type
  TConditionalFormattingDemoMainDM = class(TDataModule)
    cdsConditionalFormatting: TClientDataSet;
    cdsConditionalFormattingState: TStringField;
    cdsConditionalFormattingSales: TFloatField;
    cdsConditionalFormattingProfit: TFloatField;
    cdsConditionalFormattingSalesVsTarget: TFloatField;
    cdsConditionalFormattingMarketShare: TFloatField;
    cdsConditionalFormattingCustomersSatisfaction: TFloatField;
    dsConditionalFormatting: TDataSource;
    procedure DataModuleCreate(Sender: TObject);
  end;

var
  ConditionalFormattingDemoMainDM: TConditionalFormattingDemoMainDM;

implementation

{$R *.dfm}

{$IFDEF CLR}
uses
  Variants;
{$ENDIF}

procedure TConditionalFormattingDemoMainDM.DataModuleCreate(Sender: TObject);
begin
  cdsConditionalFormatting.LoadFromFile('..\..\Data\ConditionalFormatting.cds');
  cdsConditionalFormatting.Open;
end;

end.
