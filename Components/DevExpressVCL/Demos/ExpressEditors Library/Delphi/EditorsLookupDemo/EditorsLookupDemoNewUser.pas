unit EditorsLookupDemoNewUser;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxHyperLinkEdit, cxDBEdit, cxMaskEdit, cxControls, cxContainer,
  cxEdit, cxTextEdit, StdCtrls, cxButtons, cxDropDownEdit, cxLookupEdit,
  cxDBLookupComboBox, cxLookAndFeelPainters, cxDBLookupEdit;

type
  TEditorsLookupDemoNewUserForm = class(TForm)
    edFirstName: TcxDBTextEdit;
    edMidleName: TcxDBTextEdit;
    edLastName: TcxDBTextEdit;
    edCountry: TcxDBTextEdit;
    edCity: TcxDBTextEdit;
    mePostalCode: TcxDBMaskEdit;
    edAddress: TcxDBTextEdit;
    mePhone: TcxDBMaskEdit;
    meFax: TcxDBMaskEdit;
    heEMail: TcxDBHyperLinkEdit;
    heHomePAge: TcxDBHyperLinkEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    lcbDepartment: TcxDBLookupComboBox;
    Label12: TLabel;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function ShowEx(AName: String): Integer;
  end;

var
  EditorsLookupDemoNewUserForm: TEditorsLookupDemoNewUserForm;

implementation

uses EditorsLookupDemoData, DB;   


{$R *.dfm}

function TEditorsLookupDemoNewUserForm.ShowEx(AName: String): Integer;
var
  LName: string;
  APos: Integer;

  procedure DeleteSpaces;
  begin
    AName := Trim(AName);
    repeat
      APos := Pos('  ', AName);
      while (APos <> 0) and (AName[APos + 1] = ' ') do
        Delete(AName, APos + 1, 1);
    until APos = 0
  end;
begin
  DeleteSpaces;
  APos := Pos(' ', AName);
  if APos <> 0 then
  begin
    LName := Copy(AName, APos + 1, Length(AName) - APos);
    AName := Copy(AName, 1, APos - 1);
  end;
  with EditorsLookupDemoDataDM do
  begin
    tblUsers.Append;
    tblUsersFNAME.AsString := AName;
    tblUsersLNAME.AsString := LName;
    Result := ShowModal;
  end;

end;

procedure TEditorsLookupDemoNewUserForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  with EditorsLookupDemoDataDM do
  if ModalResult = mrOK then tblUsers.Post
  else tblUsers.Cancel;
end;

end.
