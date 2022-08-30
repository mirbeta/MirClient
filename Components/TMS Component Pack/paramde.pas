{**********************************************************
TParameter coontrol design editor
for Delphi and C++Builder

written
  TMS Software
  copyright © 2000-2012
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

unit Paramde;

interface
{$I TMSDEFS.INC}
uses
  Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


{********************************
 Property editor
 to set parameter delimiters
*********************************}
type
 TParamDelimiterProperty = class(TStringProperty)
                         public
                          function GetAttributes:TPropertyAttributes; override;
                          procedure GetValues(Proc:TGetStrProc); override;
                         end;


implementation


function TParamDelimiterProperty.GetAttributes: TPropertyAttributes;
begin
 result:=[paValueList];
end;

procedure TParamDelimiterProperty.GetValues(Proc: TGetStrProc);
begin
 proc('()');
 proc('{}');
 proc('<>');
 proc('{}');
end;


end.
 