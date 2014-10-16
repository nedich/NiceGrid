
{-------------------------------------------------------------------------------

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


     The Original Code is NiceGridReg.pas released at April 11st, 2003.
     The Original Code is a part of NiceGrid component.
     The Initial Developer of the Original Code is Priyatna.
     (Website: http://www.priyatna.org/ Email: me@priyatna.org)
     All Rights Reserved.


Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}


unit NiceGridReg;

interface

  procedure Register;

implementation

uses
  Dialogs, Classes, DesignIntf, DesignEditors, ColnEdit, NiceGrid;

type
  TNiceGridEditor = class(TComponentEditor)
  protected
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


{ TNiceGridEditor }

procedure TNiceGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of

    0: ShowCollectionEditorClass(Designer, TCollectionEditor, Component,
         TNiceGrid(Component).Columns, 'Columns', [coAdd, coDelete, coMove]);

    1: ShowMessage(
         'TNiceGrid v 1.00 (Mozilla Public License)'#13 +
         '(c) Priyatna, 2003'#13 +
         'Bandung - Indonesia'#13 +
         'http://www.priyatna.org/'#13 +
         'mailto:me@priyatna.org'
       );

  end;
end;

function TNiceGridEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit Columns ...';
    1: Result := 'About';
  end;
end;

function TNiceGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;


procedure Register; 
begin
  RegisterComponents('priyatna.org', [TNiceGrid, TNiceGridSync]);
  RegisterComponentEditor(TNiceGrid, TNiceGridEditor);
  RegisterComponentEditor(TNiceGridSync, TNiceGridEditor);
end;

end.
