{**************************************************************************}
{ TINSPECTORBAR DESIGN TIME EDITOR                                         }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1996-2012                                         }
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

unit InspDE;

interface
{$I TMSDEFS.INC}
uses
  Classes, InspectorBar, InspImg, Controls,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TInspectorBarEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TInspImageProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TInspControlProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;


implementation

uses
  SysUtils, Dialogs, ExtDlgs;

{$IFDEF DELPHI6_LVL}
procedure TInspectorBarEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TInspectorBarEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'PANELS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TInspectorBarEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
begin
  case index of
  0:begin
    {$IFDEF VER120}
    compiler := 'Delphi 4';
    {$ENDIF}
    {$IFDEF VER125}
    compiler := 'C++Builder 4';
    {$ENDIF}
    {$IFDEF VER130}
    {$IFDEF BCB}
    compiler := 'C++Builder 5';
    {$ELSE}
    compiler := 'Delphi 5';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER140}
    {$IFDEF BCB}
    compiler := 'C++Builder 6';
    {$ELSE}
    compiler := 'Delphi 6';
    {$ENDIF}
    {$ENDIF}

    {$IFDEF VER150}
    {$IFDEF BCB}
    {$ELSE}
    compiler := 'Delphi 7';
    {$ENDIF}
    {$ENDIF}

    {$IFDEF VER170}
    {$IFDEF BCB}
    {$ELSE}
    compiler := 'Delphi 2005';
    {$ENDIF}
    {$ENDIF}

    {$IFDEF VER180}
    {$IFDEF BCB}
    {$ELSE}
    compiler := 'Delphi 2006';
    {$ENDIF}
    {$ENDIF}

    {$IFDEF VER185}
    {$IFDEF BCB}
    compiler := 'C++Builder 2007';
    {$ELSE}
    compiler := 'Delphi 2007';
    {$ENDIF}
    {$ENDIF}

    {$IFDEF VER200}
      {$IFDEF BCB}
      compiler := 'C++Builder 2009';
      {$ELSE}
      compiler := 'Delphi 2009';
      {$ENDIF}
    {$ENDIF}

    MessageDlg(Component.ClassName+' version '+(Component as TInspectorBar).VersionString+' for '+compiler+#13#10'© 2001-2009 by TMS software',
               mtinformation,[mbok],0);
    end;
  1:begin
      Edit;
    end;
  2:begin
      (Component as TInspectorBar).Panels.Clear;
    end;
  end;
end;

function TInspectorBarEditor.GetVerb(index: integer): string;
begin
  case index of
  0:Result := '&Version';
  1:Result := '&Panels';
  2:Result := '&Clear';
  end;
end;

function TInspectorBarEditor.GetVerbCount: integer;
begin
  Result := 3;
end;

{ TInspImageProperty }

procedure TInspImageProperty.Edit;
var
  OpenDialog: TOpenPictureDialog;
begin
  inherited;
  OpenDialog := TOpenPictureDialog.Create(nil);

  OpenDialog.Filter:= 'All (*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf)|*.jpg;*.jpeg;*.gif;*.bmp;*.ico;*.emf;*.wmf|JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|GIF files (*.gif)|*.gif|Bitmaps (*.bmp)|*.bmp|Icons (*.ico)|*.ico|';
  OpenDialog.Filter := OpenDialog.Filter+'Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf';

  if OpenDialog.Execute then
  begin
    TInspImage(GetOrdValue).LoadFromFile(Opendialog.FileName);
  end;
  Opendialog.Free;
end;

function TInspImageProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TInspImageProperty.GetValue: String;
begin
  if not TInspImage(GetOrdValue).Empty then
    Result := '(TInspImage)'
  else
    Result := '(None)';
end;

procedure TInspImageProperty.SetValue(const Value: String);
begin
  inherited;
  if Value = '' then
    TInspImage(GetOrdValue).Assign(nil);
end;

{ TInspControlProperty }

function TInspControlProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;



procedure TInspControlProperty.GetValues(Proc: TGetStrProc);
var
  IP: TInspectorPanel;
  IB: TInspectorBar;
  i: Integer;
begin
  inherited;
  IP := (GetComponent(0) as TInspectorPanel);

  IB := IP.InspectorBar;

  for i := 1 to IB.ControlCount do
  begin
    if IB.Controls[i-1].Name <> '' then
      Proc(IB.Controls[i-1].Name);
  end;

end;

function TInspControlProperty.GetValue: String;
begin
  if GetOrdValue <> 0 then
    Result := TWinControl(GetOrdValue).Name
  else
    Result := '(None)';
end;

procedure TInspControlProperty.SetValue(const Value: String);
var
  IP: TInspectorPanel;
  IB: TInspectorBar;
  i: Integer;
begin
  inherited;

  IP := (GetComponent(0) as TInspectorPanel);

  IB := IP.InspectorBar;

  if Value = '' then
    SetOrdValue(0)
  else
    for i := 1 to IB.ControlCount do
    begin
      if IB.Controls[i-1].Name = Value then
        SetOrdValue(Integer(IB.Controls[i-1]));
    end;
end;

end.

