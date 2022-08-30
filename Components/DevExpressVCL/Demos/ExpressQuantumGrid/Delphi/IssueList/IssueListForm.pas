unit IssueListForm;

interface

uses
  Windows, Messages, Forms, SysUtils, Classes, DBCtrls, Controls, ExtCtrls,
  StdCtrls, IssueListGrid, cxControls, cxNavigator, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxLabel;

type

  TFormClass = class of TfrmBasic;

  TfrmBasic = class(TForm)
    lbCaption: TcxLabel;
    plTop: TPanel;
    cxNavigator: TcxNavigator;
  private
    FGridForm: TIssueListGridForm;
    function GetCaption: string;
    procedure SetCaption(ACaption: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Caption: string read GetCaption write SetCaption;
    property GridForm: TIssueListGridForm read FGridForm;
  end;

implementation

{$R *.dfm}

uses
  IssueListData, IssueListMain;

{ frmBasic }

constructor TfrmBasic.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Align := alClient;
  FGridForm := IssueListMainForm.GridForm;
end;

function TfrmBasic.GetCaption: string;
begin
  Result := lbCaption.Caption;
end;

procedure TfrmBasic.SetCaption(ACaption: string);
begin
  lbCaption.Caption := '  ' + ACaption;
end;

end.
