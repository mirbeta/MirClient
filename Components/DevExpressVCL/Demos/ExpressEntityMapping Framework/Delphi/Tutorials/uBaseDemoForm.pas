unit uBaseDemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutContainer, cxClasses, dxLayoutControl;


type
  { TBaseDemoForm }

  TBaseDemoFormClass = class of TBaseDemoForm;
  TBaseDemoForm = class(TForm)
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lgContent: TdxLayoutGroup;
    liDescription: TdxLayoutLabeledItem;
    lgTutorialContent: TdxLayoutGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict protected
    function GetDescription: string; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;

    procedure CheckDescription;
  public
    class procedure Register;
    class function GetCaption: string; virtual;
    class function GetGroupID: Integer; virtual;
    class function GetID: Integer; virtual;
    class function GetSortIndex: Integer; virtual;
  end;

implementation

{$R *.dfm}

uses
  uMain;

{ TBaseDemoForm }

procedure TBaseDemoForm.CheckDescription;
begin
  liDescription.CaptionOptions.Text := GetDescription;
  liDescription.Visible := liDescription.Caption <> '';
end;

function TBaseDemoForm.GetDescription: string;
begin
  Result := '';
end;

procedure TBaseDemoForm.CreateSubClasses;
begin
// do nothing
end;

procedure TBaseDemoForm.DestroySubClasses;
begin
// do nothing
end;

procedure TBaseDemoForm.FormCreate(Sender: TObject);
begin
  CreateSubClasses;
  Initialize;
end;

procedure TBaseDemoForm.FormDestroy(Sender: TObject);
begin
  Finalize;
  DestroySubClasses;
end;

procedure TBaseDemoForm.Initialize;
begin
  CheckDescription;
end;

procedure TBaseDemoForm.Finalize;
begin
// do nothing
end;

class function TBaseDemoForm.GetCaption: string;
begin
  Result := 'Base Demo Form';
end;

class function TBaseDemoForm.GetGroupID: Integer;
begin
  Result := EMFTreeViewRootID;
end;

class function TBaseDemoForm.GetID: Integer;
begin
  Result := 0;
end;

class function TBaseDemoForm.GetSortIndex: Integer;
begin
  Result := 0;
end;

class procedure TBaseDemoForm.Register;
begin
  RegisterDemo(Self);
end;

end.
