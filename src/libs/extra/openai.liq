let error.openai = error.register("openai")

openai = ()

# @flag hidden
def parse_openai_error(ans, err) =
  try
    let json.parse (e : {error: {message: string, type: string}}) = ans

    e = e.error

    error.raise(
      error.openai,
      "#{e.type}: #{e.message}"
    )
  catch _ do
    error.raise(err)
  end
end

# Query ChatGPT API.
# @param ~base_url Base URL for the API query
# @param ~key OpenAI API key.
# @param ~model Language model.
# @param ~timeout Timeout for network operations in seconds.
# @param messages Messages initially exchanged.
# @category Internet
# @flag extra
def openai.chat(
  ~key,
  ~base_url="https://api.openai.com",
  ~model="gpt-3.5-turbo",
  ~timeout=null.make(30.),
  (
  messages:
  [
    {
      role: string,
      content: string,
      name?: string,
      tool_calls?: [
        {id: string, type: string, function: {name: string, arguments: string}}
      ],
      tool_call_id?: string
    }
  ]
  )
) =
  payload = {model=model, messages=messages}

  ans =
    http.post(
      data=json.stringify(payload),
      timeout=timeout,
      headers=
        [
          ("Content-Type", "application/json"),
          (
            "Authorization",
            "Bearer #{(key : string)}"
          )
        ],
      "#{base_url}/v1/chat/completions"
    )

  if
    ans.status_code != 200
  then
    error.raise(
      error.http,
      "#{ans.status_code}: #{ans.status_message}"
    )
  end

  try
    let json.parse (ans :
      {
        choices: [
          {
            finish_reason: string,
            index: int,
            message: {content: string, role: string}
          }
        ],
        created: int,
        model: string,
        object: string,
        usage: {completion_tokens: int, prompt_tokens: int, total_tokens: int}
      }
    ) = ans
    ans
  catch err : [error.json] do
    parse_openai_error(ans, err)
  end
end

# Generate speech using openai. Returns the encoded audio data.
# @param ~base_url Base URL for the API query
# @param ~key OpenAI API key.
# @param ~model Language model.
# @param ~timeout Timeout for network operations in seconds.
# @param ~voice The voice to use when generating the audio. Supported voices are `"alloy"`, `"echo"`, `"fable"`, `"onyx"`, `"nova"`, and `"shimmer"`
# @param ~response_format The format to audio in. Supported formats are: `"mp3"`, `"opus"`, `"aac"`, and `"flac"`.
# @param ~speed The speed of the generated audio. Select a value from `0.25` to `4.0`. `1.0` is the default.
# @params ~on_data Function executed when receiving the audio data.
# @category Internet
# @flag extra
def openai.speech(
  ~key,
  ~base_url="https://api.openai.com",
  ~model="tts-1",
  ~timeout=null.make(30.),
  ~voice,
  ~response_format="mp3",
  ~speed=1.,
  ~on_data,
  (input:string)
) =
  payload =
    {
      model=model,
      input=input,
      voice=(voice : string),
      response_format=response_format,
      speed=speed
    }

  ans =
    http.post.stream(
      data=json.stringify(payload),
      timeout=timeout,
      headers=
        [
          ("Content-Type", "application/json"),
          (
            "Authorization",
            "Bearer #{(key : string)}"
          )
        ],
      on_body_data=on_data,
      "#{base_url}/v1/audio/speech"
    )

  if
    ans.status_code != 200
  then
    error.raise(
      error.http,
      "#{ans.status_code}: #{ans.status_message}"
    )
  end
end
