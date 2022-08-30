unit BaseForm;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, dxCore, cxClasses, cxLookAndFeels;

type
  TfmBaseForm = class(TForm)
    lbDescription: TLabel;
    miAbout: TMenuItem;
    miExit: TMenuItem;
    miFile: TMenuItem;
    mmMain: TMainMenu;
    procedure miAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
  protected
    procedure BuildLookAndFeelMenu; virtual;
  public
    procedure AfterConstruction; override;
  end;

var
  fmBaseForm: TfmBaseForm;

implementation

{$R *.dfm}

uses
  AboutDemoForm, DemoUtils, ActnList;

{ TfmBaseForm }

procedure TfmBaseForm.AfterConstruction;
begin
  BuildLookAndFeelMenu;
  dxBarConvertMainMenu(mmMain);
  lbDescription.Transparent := False;
  inherited AfterConstruction;
end;

procedure TfmBaseForm.BuildLookAndFeelMenu;
begin
  mmMain.Items.Insert(mmMain.Items.IndexOf(miAbout), CreateLookAndFeelMenuItems(mmMain.Items
    {$IFNDEF EXPRESSBARS}, TActionList.Create(Self) {$ENDIF}));
end;

procedure TfmBaseForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfmBaseForm.miExitClick(Sender: TObject);
begin
  Close;
end;

end.
