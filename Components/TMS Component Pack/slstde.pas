{********************************************************************
TSECTIONLISTBOX design time property editor
for Delphi & C++Builder

Copyright © 1998-2015
  TMS Software
  Email : info@tmssoftware.com
  Web : http://www.tmssoftware.com

The source code is given as is. The author is not responsible
for any possible damage done due to the use of this code.
The component can be freely used in any application. The complete
source code remains property of the author and may not be distributed,
published, given or sold in any form as such. No parts of the source
code can be included in any other component or application without
written authorization of the author.
********************************************************************}


unit slstde;

interface

{$I TMSDEFS.INC}

uses
  SlstBox, DesignIntf, DesignEditors;


type 
 {---------------------------------------------------}
 { Section property editor class : not used yet      }
 {---------------------------------------------------}
  TSectionListBoxEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;


implementation

uses
 sysutils,dialogs;

procedure TSectionListBoxEditor.ExecuteVerb(Index: integer);
var
 compiler:string;
begin
 case Index of
 0: Edit;
 1: (Component as TSectionListBox).ExpandAll;
 2: (Component as TSectionListBox).ContractAll;
 3: begin
     {$I COMPILERTEXT.INC}
     MessageDlg(Component.ClassName+' version 1.8 for ' + compiler + #13#10#13#10'© 1999-2015 by TMS software'#13#10'http://www.tmssoftware.com',
                mtInformation,[mbok],0);
   end;
 end;
end;

procedure TSectionListBoxEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'SECTIONS') = 0) then
    begin
      PropertyEditor.Edit;
      Continue := False;
    end;
end;


function TSectionListBoxEditor.GetVerb(index: integer): string;
begin
 result:='';
 case index of
 0:result:='Section Editor';
 1:result:='Expand all';
 2:result:='Contract all';
 3:result:='About';
 end;
end;

function TSectionListBoxEditor.GetVerbCount: integer;
begin
 result:=4;
end;


end.
