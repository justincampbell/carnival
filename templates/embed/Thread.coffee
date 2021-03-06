class Thread
  constructor: (@article) ->
    @createElement()
    @renderCommentForm()
    @active = false

  id: ->
    @currentBlock.id() + '-comments'

  createElement: =>
    @element = document.createElement('ol')

  setupElement: ->
    @element.id = @id()
    @element.setAttribute('data-comments-for', @currentBlock.id())
    @element.className = 'comments'

  displayForBlock: (@currentBlock) ->
    @setupElement()
    @insertCommentsIntoDom()
    @renderCommentForm()
    @article.container.insertBefore(@element, @article.container.lastChild)
    @element.style.top = @currentBlock.top() + 'px'

  comments: ->
    @currentBlock.comments

  removeAllComments: ->
    [].slice.call(@element.querySelectorAll('li.comment')).map((comment) =>
      @element.removeChild(comment)
    )

  insertCommentsIntoDom: ->
    @removeAllComments()
    for comment in @comments()
      comment.insert(@element)

  renderCommentForm: ->
    @commentForm ?= new CommentForm(this)
    @element.appendChild(@commentForm.element)

  add: (body) ->
    Carnival.post(
      '/comments',
      @commentHash(body),
      (response) =>
        comment = new Comment(response.comment)
        @comments().push(comment)
        @currentBlock.indicator.setCount(@comments().length)
        comment.insert(@element)
        @renderCommentForm()
    )

  commentHash: (body) ->
    { article: @article.id, thread: @currentBlock.id(), body: body }
