unit ViewCardDemoMain;

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxHyperLinkEdit, cxImageComboBox, cxDBLookupComboBox,
  cxMemo, cxGridCustomLayoutView, BaseForm, cxGridTableView;

type
  TViewCardDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    lvPersons: TcxGridLevel;
    cxGrid: TcxGrid;
    cvPersons: TcxGridDBCardView;
    cvPersonsID: TcxGridDBCardViewRow;
    cvPersonsFIRSTNAME: TcxGridDBCardViewRow;
    cvPersonsSECONDNAME: TcxGridDBCardViewRow;
    cvPersonsGENDER: TcxGridDBCardViewRow;
    cvPersonsBIRTHNAME: TcxGridDBCardViewRow;
    cvPersonsDATEOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvPersonsBIOGRAPHY: TcxGridDBCardViewRow;
    cvPersonsNICKNAME: TcxGridDBCardViewRow;
    cvPersonsFullname: TcxGridDBCardViewRow;
    cvPersonsHOMEPAGE: TcxGridDBCardViewRow;
    miShowEmptyRows: TMenuItem;
    miFiltering: TMenuItem;
    miExpandingCollapsing: TMenuItem;
    N1: TMenuItem;
    miRowsCustomization: TMenuItem;
    procedure miShowEmptyRowsClick(Sender: TObject);
    procedure miFilteringClick(Sender: TObject);
    procedure miExpandingCollapsingClick(Sender: TObject);
    procedure miRowsCustomizationClick(Sender: TObject);
  end;

var
  ViewCardDemoMainForm: TViewCardDemoMainForm;

implementation

{$R *.dfm}

uses
  ViewCardDemoData, Dialogs, AboutDemoForm;

procedure TViewCardDemoMainForm.miShowEmptyRowsClick(Sender: TObject);
begin
  cvPersons.OptionsView.EmptyRows := GetMenuItemChecked(Sender);
end;

procedure TViewCardDemoMainForm.miFilteringClick(Sender: TObject);
begin
  cvPersons.OptionsCustomize.RowFiltering := GetMenuItemChecked(Sender);
end;

procedure TViewCardDemoMainForm.miExpandingCollapsingClick(Sender: TObject);
begin
  cvPersons.OptionsCustomize.CardExpanding := GetMenuItemChecked(Sender);
end;

procedure TViewCardDemoMainForm.miRowsCustomizationClick(Sender: TObject);
begin
  cvPersons.Controller.Customization := True;
end;

end.
