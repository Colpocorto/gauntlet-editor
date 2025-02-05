unit uData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, LazLoggerBase;

type
  TGauntMap = array[0..31, 0..31] of integer;
  TLayer = (background, objects, positions);

  TGauntStyle = record
    id: integer;
    Name: string;
    desc: string;
  end;

  TPictureIndex = record
    id: integer;
    fileName: string;
  end;

const
  gauntStyles: array[0..7] of TGauntStyle = (
    (id: 0; Name: 'Style 1'; desc: 'Red/Yellow, chess pattern'),
    (id: 1; Name: 'Style 2'; desc: 'Dark Green/Green, brick pattern'),
    (id: 2; Name: 'Style 3'; desc: 'Blue/Light Blue, diamond pattern'),
    (id: 3; Name: 'Style 4'; desc: 'Blue/Magenta, chess pattern'),
    (id: 4; Name: 'Style 5'; desc: 'Light Red/Yellow, brick pattern'),
    (id: 5; Name: 'Style 6'; desc: 'Grey/White, diamond pattern'),
    (id: 6; Name: 'Style 7'; desc: 'Red/Yellow, diamond pattern'),
    (id: 7; Name: 'Style 8'; desc: 'Light Red/Yellow, diamond pattern')
    );
  Widths: array[0..6] of integer = (16, 24, 32, 48, 64, 96, 128);
  patternIndex: array of TPictureIndex = (
    (id: 0; fileName: 'blank.png'),
    (id: 1; fileName: 'wall00_01.png'),
    (id: 2; fileName: 'wall00_02.png'),
    (id: 3; fileName: 'wall00_03.png'),
    (id: 4; fileName: 'wall00_04.png'),
    (id: 5; fileName: 'wall00_05.png'),
    (id: 6; fileName: 'wall00_06.png'),
    (id: 7; fileName: 'wall00_07.png'),
    (id: 8; fileName: 'wall00_08.png'),
    (id: 9; fileName: 'wall00_09.png'),
    (id: 10; fileName: 'wall00_0a.png'),
    (id: 11; fileName: 'wall00_0b.png'),
    (id: 12; fileName: 'wall00_0c.png'),
    (id: 13; fileName: 'wall00_0d.png'),
    (id: 14; fileName: 'wall00_0e.png'),
    (id: 15; fileName: 'wall00_0f.png')

    );

var
  mapData, visitedData: TGauntMap;
  gauntStyle: TGauntStyle;
  ilMap: TImageList;
  ilTools: TImageList;
  resourcesDir: string;
  patternIndexMap: array [0..$48 * 6 - 1] of integer;

procedure initdata;
procedure initMap(var map: TGauntMap);
procedure loadGraphics(AOwner: TComponent);

implementation

procedure initdata;
begin
  gauntStyle := gauntStyles[0];
  resourcesDir := ExtractFilePath(ParamStr(0));
end;

procedure loadGraphics(AOwner: TComponent);
var
  tmpPng: TPortableNetworkGraphic;
  i: integer;
begin
  //Create objects
  ilMap := TImageList.Create(AOwner);
  ilMap.RegisterResolutions(Widths);
  ilTools := TImageList.Create(AOwner);
  tmpPng := TPortableNetworkGraphic.Create;


  //Load map objects
  for i := 0 to length(patternIndex) do
  begin
    try
      tmpPng.LoadFromFile(resourcesDir + patternIndex[i].fileName);
      patternIndexMap[patternIndex[i].id] := ilMap.Add(tmpPng, nil);
    except
      on E: Exception do DebugLn('Error loading ' + patternIndex[i].fileName + ': ' + E.Message);
    end;

  end;
end;

procedure initMap(var map: TGauntMap);
var
  x, y: integer;
begin
  for x := 0 to 31 do
    for y := 0 to 31 do
      map[x, y] := 0;
end;

end.
