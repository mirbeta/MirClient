{*********************************************************************}
{ TTreeList component                                                 }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1996-2012                                    }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{*********************************************************************}

unit tlreg;

{$I TMSDEFS.INC}
interface

uses
 treelist,classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TTreeList]);
end;

end.

