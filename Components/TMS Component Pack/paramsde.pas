{**************************************************************************}
{ Parameter control design time property editor interface                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2012                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit paramsde;

interface
{$I TMSDEFS.INC}
uses
  ParamProp, ParamListProp, ParamTreeProp, Classes, Forms, Dialogs, Controls, Windows, TypInfo, Graphics,
  PictureContainer,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;
  type

  TParamStringProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TParamStringListProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TParamNodesProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TParamDefaultEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
  end;

  TParamListDefaultEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
  end;
implementation

uses
  SysUtils;

{ TParamDefaultEditor }
{$IFDEF DELPHI6_LVL}
procedure TParamDefaultEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
{$ELSE}
procedure TParamDefaultEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
 PropName: string;
begin
{$IFDEF DELPHI6_LVL}
 PropName := Prop.GetName;
{$ELSE}
 PropName := PropertyEditor.GetName;
{$ENDIF}
 if (CompareText(PropName, 'HTMLTEXT') = 0) then
   begin
{$IFDEF DELPHI6_LVL}
    Prop.Edit;
{$ELSE}
    PropertyEditor.Edit;
{$ENDIF}
    Continue := False;
   end;
end;


{ TParamDefaultEditor }
{$IFDEF DELPHI6_LVL}
procedure TParamListDefaultEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
{$ELSE}
procedure TParamListDefaultEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
{$IFDEF DELPHI6_LVL}
  PropName := Prop.GetName;
{$ELSE}
  PropName := PropertyEditor.GetName;
{$ENDIF}
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
{$IFDEF DELPHI6_LVL}
    Prop.Edit;
{$ELSE}
    PropertyEditor.Edit;
{$ENDIF}
    Continue := False;
  end;
end;

function TParamStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TParamStringProperty.Edit;
var
  ParamEditor: TParamEditor;
  PropInfo: PPropInfo;

begin
  ParamEditor := TParamEditor.Create(Application);

  try
    ParamEditor.Memo1.Lines.Assign(TStrings(GetOrdValue));

    {try to inherit the default font}
    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Font');
    if (PropInfo<>nil) then
       ParamEditor.paramlabel1.Font.Assign(TFont(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Images');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.Images := (TImageList(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamColor');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ParamColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverColor');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.HoverColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverFontColor');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.HoverFontColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Hover');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.Hover := (boolean(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowColor');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ShadowColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowOffset');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ShadowOffset := (integer(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'LineSpacing');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.LineSpacing := (integer(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShowHint');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ShowHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));


    ParamEditor.DBfields.Visible:=false;

    if ParamEditor.Showmodal = mrOk then
    begin
      SetOrdValue(Longint(ParamEditor.Memo1.Lines));

    end;
  finally
    ParamEditor.Free;
  end;
end;

procedure TParamStringProperty.SetValue(const Value: String);
begin
end;

function TParamStringProperty.GetValue: String;
begin
  Result := '(ParamString)';
end;



function TParamStringListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TParamStringListProperty.Edit;
var
  ParamEditor: TParamListEditor;
  PropInfo: PPropInfo;

begin
  ParamEditor := TParamListEditor.Create(Application);

  try
    ParamEditor.ParamListBox1.Items.Assign(TStrings(GetOrdValue));
    {try to inherit the default font}
    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Font');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Font.Assign(TFont(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.Font.Assign(TFont(GetOrdProp(GetComponent(0),PropInfo)));

    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Images');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Images:=(TImageList(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.Images:=(TImageList(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ParamColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ParamHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlabel1.ParamHint:=(boolean(GetObjectProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ParamHint:=(boolean(GetObjectProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShowHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShowHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ShowHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlabel1.ShowHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ShowHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.HoverColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.HoverColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverFontColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.HoverFontColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.HoverFontColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Hover');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Hover:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.Hover:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));

    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ParamHint:=(boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShadowColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ShadowColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowOffset');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShadowOffset:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.ShadowOffset:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Images');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Images := (TImageList(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.Images := (TImageList(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'LineSpacing');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.LineSpacing:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramlistbox1.LineSpacing:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ItemHeight');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlistbox1.ItemHeight := (integer(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'SelectionColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlistbox1.SelectionColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'SelectionFontColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlistbox1.SelectionFontColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShowSelection');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlistbox1.ShowSelection := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    {
    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'PictureContainer');
    if (PropInfo<>nil) then
      ParamEditor.paramlabel1.PictureContainer := (TPictureContainer(GetOrdProp(GetComponent(0),PropInfo)));
    }

    ParamEditor.DBfields.Visible:=false;

    if ParamEditor.ParamListBox1.Items.Count > 0 then
      ParamEditor.ParamListBox1.ItemIndex := 0;

    if ParamEditor.Showmodal = mrOk then
    begin
      SetOrdValue(longint(ParamEditor.ParamListBox1.Items));

    end;
  finally
    ParamEditor.Free;
  end;
end;

procedure TParamStringListProperty.SetValue(const Value: String);
begin
end;

function TParamStringListProperty.GetValue: String;
begin
  Result := '(ParamStrings)';
end;

function TParamNodesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TParamNodesProperty.Edit;
var
  ParamEditor: TParamTreeEditor;
  PropInfo: PPropInfo;
begin
  ParamEditor := TParamTreeEditor.Create(Application);
  try
    ParamEditor.ParamTreeView1.Items.Assign(TStrings(GetOrdValue));
    {try to inherit the default font}
    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Font');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Font.Assign(TFont(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.Font.Assign(TFont(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Color');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramtreeview1.Color := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ItemHeight');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramtreeview1.ItemHeight := GetOrdProp(GetComponent(0),PropInfo);
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HTMLImages');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Images := (TImageList(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.HTMLImages := (TImageList(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.ParamColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShowHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShowHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.ShowHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.HoverColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.ParamTreeview1.HoverColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'HoverFontColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.HoverFontColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.HoverFontColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));

    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'Hover');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.Hover := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.ParamTreeview1.Hover := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ParamHint');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.ParamTreeview1.ParamHint := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShadowColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.ShadowColor:=(TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShadowOffset');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.ShadowOffset:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.ShadowOffset:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'LineSpacing');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramlabel1.LineSpacing:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
      ParamEditor.paramtreeview1.LineSpacing:=(integer(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'SelectionColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramtreeview1.SelectionColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'SelectionFontColor');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramtreeview1.SelectionFontColor := (TColor(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    PropInfo := typInfo.GetPropInfo(GetComponent(0).ClassInfo,'ShowSelection');
    if (PropInfo<>nil) then
    begin
      ParamEditor.paramtreeview1.ShowSelection := (boolean(GetOrdProp(GetComponent(0),PropInfo)));
    end;

    ParamEditor.Paramtreeview1.HideSelection := False;

    ParamEditor.DBfields.Visible:=false;

//    if ParamEditor.ParamListBox1.Items.Count > 0 then
//      ParamEditor.ParamListBox1.ItemIndex := 0;

    if ParamEditor.Showmodal = mrOk then
    begin
      SetOrdValue(longint(ParamEditor.ParamTreeView1.Items));
    end;
  finally
    ParamEditor.Free;
  end;
end;

procedure TParamNodesProperty.SetValue(const Value: String);
begin
end;

function TParamNodesProperty.GetValue: String;
begin
  Result := '(ParamStrings)';
end;

end.
