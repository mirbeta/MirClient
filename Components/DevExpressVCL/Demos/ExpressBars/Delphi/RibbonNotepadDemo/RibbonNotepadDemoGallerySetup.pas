unit RibbonNotepadDemoGallerySetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TColorDialogSetupForm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    chkRemoveHorizontalItemPadding: TCheckBox;
    chkRemoveVerticalItemPadding: TCheckBox;
  private
    { Private declarations }
  public
    function GetSettings(var RemoveHorizontalItemPadding,
      RemoveVerticalItemPadding: Boolean): Boolean;
  end;

var
  ColorDialogSetupForm: TColorDialogSetupForm;

implementation

{$R *.dfm}

function TColorDialogSetupForm.GetSettings(var RemoveHorizontalItemPadding,
  RemoveVerticalItemPadding: Boolean): Boolean;
begin
  chkRemoveHorizontalItemPadding.Checked := RemoveHorizontalItemPadding;
  chkRemoveVerticalItemPadding.Checked := RemoveVerticalItemPadding;

  Result := ShowModal = mrOK;

  if Result then
  begin
    RemoveHorizontalItemPadding := chkRemoveHorizontalItemPadding.Checked;
    RemoveVerticalItemPadding := chkRemoveVerticalItemPadding.Checked;
  end;
end;

end.
