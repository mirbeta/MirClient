{***************************************************************************}
{ TAdvMetroCategoryList design time support                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvMetroCategoryListRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, GDIPicDE, AdvMetroCategoryList, GDIPicture, ImgList,
  DesignIntf, DesignEditors
  {$IFDEF DELPHIXE3_LVL}
  , UITypes
  {$ENDIF}
  ;

type
  TAdvMetroCategoryListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  end;



procedure Register;

implementation

uses
  SysUtils;

{ TAdvMetroCategoryListEditor }

procedure TAdvMetroCategoryListEditor.EditProperty(const Prop: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'Categories') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TAdvMetroCategoryList, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TCategory, '', TGDIPPictureProperty);
  RegisterPropertyEditor(TypeInfo(TGDIPPicture), TCategoryItem, '', TGDIPPictureProperty);
  RegisterComponentEditor(TAdvMetroCategoryList, TAdvMetroCategoryListEditor);
end;


end.
