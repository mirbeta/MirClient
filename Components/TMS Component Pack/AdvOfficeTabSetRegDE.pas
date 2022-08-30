unit AdvOfficeTabSetRegDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, AdvOfficeTabSet, GDIPicture, GDIPicDE,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  // Setting property Editor to all properties of type TGDIPPicture
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TTabSetButtonSettings, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TOfficeTabCollectionItem, 'Picture', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TOfficeTabCollectionItem, 'DisabledPicture', TGDIPPictureProperty);
end;



end.

