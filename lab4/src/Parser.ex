defmodule Parser do
  def parseline(linestring) do
    [dateTime, type, value, stationID, stationName, coordinates] = linestring |> String.split(";")

    %{
      dateTime: {parseDate(dateTime),parseTime(dateTime)},
      type: type,
      value: value |> String.to_float,
      stationID: stationID,
      stationName: stationName,
      coordinates: parseCoordinates(coordinates) 
      }

  end
  
  defp parseDate(dateTime) do
    dateTime
    |> String.slice(0..9)
    |> String.split("-")
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end
    
  defp parseTime(dateTime) do
    dateTime
    |> String.slice(11..18)
    |> String.split(":")
    |> Enum.map(&String.to_integer/1)  
    |> List.to_tuple()
  end
    
  defp parseCoordinates(coordinates) do
    coordinates 
    |> String.split(",") 
    |> Enum.map(&String.to_float()/1) 
    |> List.to_tuple()
  end
end

defmodule Reader do
  def readFile(path \\ "../data/AirlyData-ALL-50k.csv" ) do
    File.read!(path) 
  end

  def parseFile(content) do 
    content 
    |> String.trim()
    |> String.split("\n")  
    |> Enum.map(&Parser.parseline/1)
    # |> length() 
  end 
end

defmodule Loader do
  def init do
  "./app/_build/default/lib/app/ebin" |> Code.append_path
  Application.start(:app)
  end
  
  def uploadStations() do
  Reader.readFile()
  |> Reader.parseFile()
  |> Enum.map(fn line -> %{stationName: "#{Map.fetch!(line, :stationID)} #{Map.fetch!(line, :stationName)}", coordinates: Map.fetch!(line, :coordinates) } end )
  |> Enum.uniq_by(fn (lineMap) -> Map.fetch(lineMap, :coordinates) end)
  |> Enum.uniq_by(fn (lineMap) -> Map.fetch(lineMap, :stationName) end)
  |> Enum.map(fn line ->
  :pollution_gen_server.add_station(
    Map.fetch!(line, :stationName),
    Map.fetch!(line, :coordinates)
  )
end)
  end

  def uploadReadings() do
    Reader.readFile()
    |> Reader.parseFile()
    |> Enum.map(
      fn line -> 
        :pollution_gen_server.add_value(Map.fetch!(line, :coordinates), Map.fetch!(line, :dateTime), Map.fetch!(line, :type), Map.fetch!(line, :value))
      end 
    )
  end

  def uploadReadingsTimed do
    {time, _} = :timer.tc(fn -> uploadReadings() end)
    IO.puts("Readings:\t#{time/1000000} s")
  end

  def uploadStationsTimed do
    {time,_} = :timer.tc(fn -> uploadStations() end)
    IO.puts("Stations:\t#{time/1000000} s")
  end

  def firstTest do
    {time, result} = :timer.tc(fn ->
      :pollution_gen_server.get_station_min("9910 Polska, Kraków, Studencka", "PM10")
    end)
    IO.puts("firstTest result: #{inspect(result)} (took #{time/1000000} s)")
    result
  end

  def secondTest do
    {time, result} = :timer.tc(fn ->
    :pollution_gen_server.get_daily_mean({2024,2,10}, "PM25")
    end)
    IO.puts("secondTest result: #{inspect(result)} (took #{time/1000000} s)")
    result
  end

  def integrationTest do
    stop()
    init()
    uploadStationsTimed()
    uploadReadingsTimed()
    firstTest()
    secondTest()
    stop()
    init()
  end

  def stop do
    Application.stop(:app)
  end

  def loaderTest() do
    init()
    uploadStations()
    uploadReadingsTimed()
  end

end

