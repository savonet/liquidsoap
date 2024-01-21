let error.chatgpt = error.register("chatgpt")

# Query ChatGPT API.
# @param ~base_url Base URL for the API query
# @param ~key OpenAI API key.
# @param ~model Language model.
# @param messages Messages initially exchanged.
def chatgpt(
  ~key,
  ~base_url="https://api.openai.com",
  ~model="gpt-3.5-turbo",
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
    try
      let json.parse (e :
        {error: {message: string, type: string, code: string}}
      ) = ans
      e = e.error
      error.raise(
        error.chatgpt,
        "#{e.code}: #{e.message}"
      )
    catch _ do
      error.raise(err)
    end
  end
end
