{*************************************************************************}
{ HTMLEditor property type                                                }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2009                                             }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSmoothHTMLProp;

interface

{$I TMSDEFS.INC}

uses
  AdvSmoothHTMLEditor, Classes, Forms, Dialogs, Controls, Windows, TypInfo,
  Graphics, Sysutils, GDIPPictureContainer, 
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TAdvSmoothHTMLProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation


function TAdvSmoothHTMLProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TAdvSmoothHTMLProperty.Edit;
var
  HTMLEditor: TAdvSmoothHTMLEditorForm;
  PropInfo: PPropInfo;
  S: string;
  i: Integer;
  comp: TPersistent;
  {$IFDEF DELPHI6_LVL}  
  pcomp: TPersistent;
  fcomp: TComponent;
  {$ENDIF}
begin
  HTMLEditor := TAdvSmoothHTMLEditorForm.Create(Application);
  try
    HTMLEditor.Memo1.Lines.Clear;
    S := String(GetStrValue);
    if S <> '' then
      HTMLEditor.Memo1.Lines.Add(s);


    comp := GetComponent(0);
    {$IFDEF DELPHI6_LVL}
    fcomp := TComponent(GetComponent(0));
    {$ENDIF}

    //try to inherit the default font
    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'Font');
    if (PropInfo<>nil) then
    begin
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLFont.Assign(TFont(GetOrdProp(comp,PropInfo)));
      HTMLEditor.Memo1.Font.Assign(TFont(GetOrdProp(comp,PropInfo)));
      HTMLEditor.Memo1.Font.Color := clBlack;
    end;

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'HTMLFont');
    if (PropInfo<>nil) then
    begin
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLFont.Assign(TFont(GetOrdProp(comp,PropInfo)));
      HTMLEditor.Memo1.Font.Assign(TFont(GetOrdProp(comp,PropInfo)));
      HTMLEditor.Memo1.Font.Color := clBlack;
    end;

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'Images');
    if (PropInfo<>nil) then
      HTMLEditor.AdvSmoothPanel1.Images:=(TImageList(GetOrdProp(comp,PropInfo)));

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'HTMLURLColor');
    if (PropInfo<>nil) then
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLURLColor:=(TColor(GetOrdProp(comp,PropInfo)));

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'URLColor');
    if (PropInfo<>nil) then
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLURLColor:=(TColor(GetOrdProp(comp,PropInfo)));

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'NotesURLColor');
    if (PropInfo<>nil) then
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLURLColor:=(TColor(GetOrdProp(comp,PropInfo)));

    PropInfo := typInfo.GetPropInfo(comp.ClassInfo,'CaptionURLColor');
    if (PropInfo<>nil) then
      HTMLEditor.AdvSmoothPanel1.Caption.HTMLURLColor:=(TColor(GetOrdProp(comp,PropInfo)));


    {$IFDEF DELPHI6_LVL}
    if comp is TCollectionItem then
      pcomp := (comp as TCollectionItem).Collection.Owner
    else
      pcomp := fcomp.Owner;

    if Assigned(pcomp) then
    begin
      PropInfo := typInfo.GetPropInfo(pcomp.ClassInfo,'Images');
      if (PropInfo<>nil) then
        HTMLEditor.AdvSmoothPanel1.Images:=(TImageList(GetOrdProp(pcomp,PropInfo)));

      PropInfo := typInfo.GetPropInfo(pcomp.ClassInfo,'PictureContainer');
      if PropInfo <> nil then
        HTMLEditor.AdvSmoothPanel1.PictureContainer := (TGDIPPictureContainer(GetOrdProp(pcomp,PropInfo)));
    end;
    {$ENDIF}    

    if HTMLEditor.Showmodal = mrOK then
    begin
      s := '';
      for i := 1 to HTMLEditor.Memo1.Lines.Count do
      begin
        if i = 1 then
          s := HTMLEditor.Memo1.Lines[i - 1]
        else
        begin
          if pos('<BR>',Uppercase(HTMLEditor.Memo1.Lines[i - 2])) = Length(HTMLEditor.Memo1.Lines[i - 2]) - 3 then
            s := s + HTMLEditor.Memo1.Lines[i - 1]
          else
            s := s + ' ' + HTMLEditor.Memo1.Lines[i - 1];
        end;
      end;
      SetStrValue(s);
    end;

  finally
    HTMLEditor.Free;
  end;
end;

procedure TAdvSmoothHTMLProperty.SetValue(const Value: String);
begin

end;

function TAdvSmoothHTMLProperty.GetValue: String;
begin
  Result:='(HTMLString)';
end;


end.
