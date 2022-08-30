{**************************************************************************}
{ HTML design time property editor interface                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000 - 2013                                       }
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

unit PictureContainerDE;

interface

{$I TMSDEFS.INC}

uses
  Classes,Forms,Dialogs,Controls,Windows,TypInfo,Graphics,Sysutils,
  PictureContainerProp, PictureContainer, DesignIntf, DesignEditors;

type
  TPictureContainerDefaultEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const Prop:IProperty; var Continue:Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TPictureContainerProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

implementation

function TPictureContainerProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

procedure TPictureContainerProperty.Edit;
var
  ContainerEditor:TContainerEditor;

begin
  ContainerEditor := TContainerEditor.Create(Application);
  try
    ContainerEditor.PictureContainer.Items.Assign(TPictureCollection(GetOrdValue));

    ContainerEditor.UpdateList;

    if ContainerEditor.Showmodal = mrOK then
    begin
      TPictureCollection(GetOrdValue).Assign(ContainerEditor.PictureContainer.Items);
      Modified;
    end;

  finally
    ContainerEditor.Free;
  end;
end;

procedure TPictureContainerProperty.SetValue(const Value: String);
begin
end;

function TPictureContainerProperty.GetValue: String;
begin
  Result := '(Container)';
end;



{ THTMLDefaultEditor }
procedure TPictureContainerDefaultEditor.EditProperty(const Prop:IProperty; var Continue:Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    Prop.Edit;
    Continue := False;
  end;
end;



procedure TPictureContainerDefaultEditor.ExecuteVerb(Index: integer);
var
 compiler:string;
begin
  case Index of
  0:Edit;
  1:begin
     {$I COMPILERTEXT.INC}

     MessageDlg(Component.ClassName+' for '+Compiler+#13#10#13#10'© 1999-2015 by TMS software'#13#10'http://www.tmssoftware.com',
               mtInformation,[mbok],0);
    end;
  end;
end;



function TPictureContainerDefaultEditor.GetVerb(Index: integer): string;
begin
  Result := '';
  case Index of
  0:Result := 'Container Editor';
  1:Result := 'About';
  end;
end;

function TPictureContainerDefaultEditor.GetVerbCount: integer;
begin
  Result := 2;
end;

end.
